/*
 * This file and its contents are supplied under the terms of the
 * Common Development and Distribution License ("CDDL"), version 1.0.
 * You may only use this file in accordance with the terms of version
 * 1.0 of the CDDL.
 *
 * A full copy of the text of the CDDL should have accompanied this
 * source.  A copy of the CDDL is also available via the Internet at
 * http://www.illumos.org/license/CDDL.
 */

/*
 * Copyright (C) 2013 Hewlett-Packard Development Company, L.P.
 * Copyright 2016 Joyent, Inc.
 */

#include <sys/sdt.h>
#include "cpqary3.h"

/*
 * Local Functions Definitions
 */

static boolean_t cpqary3_is_scsi_read_write(struct scsi_pkt *scsi_pktp);

/*
 * External Variable Declarations
 */

extern ddi_dma_attr_t cpqary3_dma_attr;

int
cpqary3_getcap(struct scsi_address *sa, char *capstr, int tgtonly)
{
	scsi_hba_tran_t *tran = sa->a_hba_tran;
	cpqary3_t *cpq = (cpqary3_t *)tran->tran_hba_private;
	int index;

	/*
	 * If requested Capability is not supported, return -1.
	 */
	if ((index = scsi_hba_lookup_capstr(capstr)) == DDI_FAILURE) {
		return (CAP_NOT_DEFINED);
	}

	/*
	 * Getting capability for a particulat target is supported
	 * the generic form of tran_getcap() is unsupported(for all targets)
	 * If directed towards a particular target, return current capability.
	 */
	if (tgtonly == 0) {	/* all targets */
		DTRACE_PROBE1(getcap_alltgt, int, index);
		return (CAP_NOT_DEFINED);
	}

	DTRACE_PROBE1(getcap_index, int, index);

	switch (index) {
	case SCSI_CAP_CDB_LEN:
		return (CPQARY3_CDBLEN_16);
	case SCSI_CAP_DMA_MAX:
		return ((int)cpq->cpq_dma_attr.dma_attr_maxxfer);
	case SCSI_CAP_DISCONNECT:
	case SCSI_CAP_SYNCHRONOUS:
	case SCSI_CAP_WIDE_XFER:
	case SCSI_CAP_ARQ:
		return (1);
	case SCSI_CAP_INITIATOR_ID:
		return (CPQARY3_CONTROLLER_TARGET);
	case SCSI_CAP_UNTAGGED_QING:
		return (1);
	case SCSI_CAP_TAGGED_QING:
		return (1);
	case SCSI_CAP_SECTOR_SIZE:
		return ((int)cpq->cpq_dma_attr.dma_attr_granular);
	case SCSI_CAP_TOTAL_SECTORS:
		return (CAP_NOT_DEFINED);
#if 0 /* XXX: pks: Don't override this for physdevs, use SCSA default */
	case SCSI_CAP_GEOMETRY:
		return (cpqary3_target_geometry(sa));
#endif
	case SCSI_CAP_RESET_NOTIFICATION:
		return (0);
	case SCSI_CAP_INTERCONNECT_TYPE:
		if (cpq->cpq_board->bd_flags & SA_BD_SAS) {
			/* XXX: make sd qualify as DDI_NT_BLOCK_WWN */
			return (INTERCONNECT_FABRIC);
			/* return (INTERCONNECT_SAS); */
		} else {
			return (INTERCONNECT_PARALLEL);
		}
		break;
	case SCSI_CAP_SCSI_VERSION:
		if (cpq->cpq_board->bd_flags & SA_BD_SAS) {
			return (SCSI_VERSION_3);
		} else {
			return (SCSI_VERSION_2); /* XXX check */
		}
		break;
	default:
		return (CAP_NOT_DEFINED);
	}
}

/* ARGSUSED */
int
cpqary3_setcap(struct scsi_address *sa, char *capstr, int value, int tgtonly)
{
	int	index;
	int	retstatus = CAP_NOT_DEFINED;

	/*
	 * If requested Capability is not supported, return -1.
	 */
	if ((index = scsi_hba_lookup_capstr(capstr)) == DDI_FAILURE)
		return (retstatus);

	/*
	 * Setting capability for a particulat target is supported
	 * the generic form of tran_setcap() is unsupported(for all targets)
	 * If directed towards a particular target, set & return current
	 * capability.
	 */
	if (!tgtonly) {
		DTRACE_PROBE1(setcap_alltgt, int, index);
		return (retstatus);
	}

	DTRACE_PROBE1(setcap_index, int, index);

	switch (index) {
	case SCSI_CAP_CDB_LEN:
	case SCSI_CAP_DMA_MAX:
	case SCSI_CAP_DISCONNECT:
	case SCSI_CAP_SYNCHRONOUS:
	case SCSI_CAP_WIDE_XFER:
	case SCSI_CAP_ARQ:
	case SCSI_CAP_INITIATOR_ID:
	case SCSI_CAP_SECTOR_SIZE:
	case SCSI_CAP_TOTAL_SECTORS:
	case SCSI_CAP_UNTAGGED_QING:
	case SCSI_CAP_TAGGED_QING:
#if 0
	case SCSI_CAP_GEOMETRY:
#endif
	case SCSI_CAP_RESET_NOTIFICATION:
		return (CAP_CHG_NOT_ALLOWED);

	default:
		return (CAP_NOT_DEFINED);
	}
}

void
cpqary3_oscmd_complete(cpqary3_command_t *cpcm)
{
	cpqary3_t	*cpqary3p = cpcm->cpcm_ctlr;
	ErrorInfo_t	*errorinfop = cpcm->cpcm_va_err;
	struct scsi_pkt	*scsi_pktp = cpcm->cpcm_scsa->cpcms_pkt;

	VERIFY(MUTEX_HELD(&cpqary3p->cpq_mutex));
	VERIFY(cpcm->cpcm_type == CPQARY3_CMDTYPE_SCSA);

	if (cpcm->cpcm_status & CPQARY3_CMD_STATUS_RESET_SENT) {
		if (scsi_pktp->pkt_reason != CMD_CMPLT) {
			/*
			 * If another error status was previously written,
			 * do not overwrite it.
			 */
			scsi_pktp->pkt_reason = CMD_RESET;
		}
		scsi_pktp->pkt_statistics |= STAT_BUS_RESET | STAT_DEV_RESET;
		goto finish;
	}

	if (!(cpcm->cpcm_status & CPQARY3_CMD_STATUS_ERROR)) {
		scsi_pktp->pkt_state |= STATE_XFERRED_DATA | STATE_GOT_STATUS;
		goto finish;
	}

	switch (errorinfop->CommandStatus) {
	case CISS_CMD_DATA_OVERRUN:
		scsi_pktp->pkt_reason = CMD_DATA_OVR;
		scsi_pktp->pkt_state |= STATE_XFERRED_DATA | STATE_GOT_STATUS;
		break;

	case CISS_CMD_INVALID:
		DTRACE_PROBE1(invalid_cmd, struct scsi_pkt *, scsi_pktp);
		scsi_pktp->pkt_reason = CMD_BADMSG;
		scsi_pktp->pkt_state |= STATE_GOT_STATUS;
		break;

	case CISS_CMD_PROTOCOL_ERR :
		scsi_pktp->pkt_reason = CMD_BADMSG;
		scsi_pktp->pkt_state |= STATE_GOT_STATUS;
		break;

	case CISS_CMD_HARDWARE_ERR:
	case CISS_CMD_CONNECTION_LOST:
		scsi_pktp->pkt_reason = CMD_INCOMPLETE;
		scsi_pktp->pkt_state = 0; /* XXX ? */
		break;

	/*
	 * The controller has reported completion for a command in response
	 * to an abort message.
	 */
	case CISS_CMD_ABORTED:
	case CISS_CMD_UNSOLICITED_ABORT:
		if (cpcm->cpcm_status & CPQARY3_CMD_STATUS_TIMEOUT) {
			/*
			 * This abort was arranged by the periodic routine
			 * in response to an elapsed timeout.
			 */
			scsi_pktp->pkt_reason = CMD_TIMEOUT;
			scsi_pktp->pkt_statistics |= STAT_TIMEOUT;
		} else {
			scsi_pktp->pkt_reason = CMD_ABORTED;
		}
		scsi_pktp->pkt_statistics |= STAT_ABORTED;
		scsi_pktp->pkt_state |= STATE_XFERRED_DATA | STATE_GOT_STATUS;
		break;

	case CISS_CMD_ABORT_FAILED:
		break;

	/*
	 * The controller suggests that the timeout we specified in the
	 * SCSI packet has expired.
	 */
	case CISS_CMD_TIMEOUT:
		scsi_pktp->pkt_reason = CMD_TIMEOUT;
		scsi_pktp->pkt_statistics |= STAT_TIMEOUT;
		break;

	case CISS_CMD_DATA_UNDERRUN:	/* Significant ONLY for Read & Write */
		if (cpqary3_is_scsi_read_write(scsi_pktp)) {
			scsi_pktp->pkt_reason = CMD_CMPLT;
			scsi_pktp->pkt_statistics = 0;
			scsi_pktp->pkt_state =
			    STATE_GOT_BUS | STATE_GOT_TARGET | STATE_SENT_CMD |
			    STATE_XFERRED_DATA | STATE_GOT_STATUS;
			break;
		}
		/* FALLTHROUGH */
	case CISS_CMD_SUCCESS:
	case CISS_CMD_TARGET_STATUS:
		scsi_pktp->pkt_reason = CMD_CMPLT;
		scsi_pktp->pkt_statistics = 0;
		scsi_pktp->pkt_state = STATE_GOT_BUS | STATE_GOT_TARGET |
		    STATE_SENT_CMD | STATE_XFERRED_DATA | STATE_GOT_STATUS;
		break;

	default:	/* Should never Occur !!! */
		scsi_pktp->pkt_reason = CMD_TRAN_ERR;
		break;
	}


	/*
	 * if ever a command completes with a CHECK CONDITION or a
	 * COMMAND_TERMINATED SCSI status, Update the sense data.
	 * NOTE : The CISS_CMD_INVALID command status would always result in a
	 * CHECK CONDITION and hence reach this part of the code.
	 */

	if ((errorinfop->ScsiStatus == SCSI_CHECK_CONDITION) ||
	    (errorinfop->ScsiStatus == SCSI_COMMAND_TERMINATED)) {
		if (errorinfop->SenseLen) {
			struct scsi_arq_status	*arq_statusp;
			arq_statusp =
			    /* LINTED: alignment */
			    (struct scsi_arq_status *)scsi_pktp->pkt_scbp;

			if ((errorinfop->ScsiStatus == SCSI_CHECK_CONDITION)) {
				arq_statusp->sts_status.sts_chk = (uint8_t)1;
			} else {
				arq_statusp->sts_status.sts_chk = (uint8_t)1;
				arq_statusp->sts_status.sts_scsi2 = (uint8_t)1;
			}
			bzero((void *)&(arq_statusp->sts_rqpkt_status),
			    sizeof (struct scsi_status));
			arq_statusp->sts_rqpkt_reason = CMD_CMPLT;
			arq_statusp->sts_rqpkt_resid = 0;
			arq_statusp->sts_rqpkt_state = scsi_pktp->pkt_state;
			arq_statusp->sts_rqpkt_statistics =
			    scsi_pktp->pkt_statistics;
			bcopy((caddr_t)&errorinfop->SenseInfo[0],
			    (caddr_t)(&arq_statusp->sts_sensedata),
			    MIN(errorinfop->SenseLen,
			    cpcm->cpcm_scsa->cpcms_pkt->pkt_scblen));
			scsi_pktp->pkt_state |= STATE_ARQ_DONE;
		}
	}

finish:
	mutex_exit(&cpqary3p->cpq_mutex);
	scsi_hba_pkt_comp(scsi_pktp);
	mutex_enter(&cpqary3p->cpq_mutex);
}

static boolean_t
cpqary3_is_scsi_read_write(struct scsi_pkt *scsi_pktp)
{
	/*
	 * In the scsi packet structure, the first byte is the SCSI Command
	 * OpCode.  We check to see if it is any one of the SCSI Read or Write
	 * opcodes.
	 */
	switch (scsi_pktp->pkt_cdbp[0]) {
	case SPC3_CMD_READ6:
	case SPC3_CMD_READ10:
	case SPC3_CMD_READ12:
	case SPC3_CMD_WRITE6:
	case SPC3_CMD_WRITE10:
	case SPC3_CMD_WRITE12:
		return (B_TRUE);

	default:
		return (B_FALSE);
	}
}
