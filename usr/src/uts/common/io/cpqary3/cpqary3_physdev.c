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

#include "cpqary3.h"

cpqary3_phys_dev_t *
cpqary3_lookup_phys_dev_by_physid(cpqary3_t *cpq, PhysDevAddr_t *pdp)
{
	VERIFY(MUTEX_HELD(&cpq->cpq_mutex));

	if (pdp == NULL)
		return (NULL);

	for (cpqary3_phys_dev_t *cppd = list_head(&cpq->cpq_phys_devs);
	    cppd != NULL; cppd = list_next(&cpq->cpq_phys_devs, cppd)) {
		if (memcmp(&(cppd->cppd_addr), pdp,
			sizeof (PhysDevAddr_t)) == 0) {
			return (cppd);
		}
	}

	return (NULL);
}

cpqary3_phys_dev_t *
cpqary3_lookup_phys_dev_by_id(cpqary3_t *cpq, unsigned id)
{
	VERIFY(MUTEX_HELD(&cpq->cpq_mutex));

	for (cpqary3_phys_dev_t *cppd = list_head(&cpq->cpq_phys_devs);
	    cppd != NULL; cppd = list_next(&cpq->cpq_phys_devs, cppd)) {
		if (cppd->cppd_addr.TargetId == id) {
		/* if (cppd->cppd_addr.Target[1].LogUnit.Targ == id) { */
			return (cppd);
		}
	}

	return (NULL);
}

cpqary3_phys_dev_t *
cpqary3_lookup_phys_dev_by_addr(cpqary3_t *cpq, struct scsi_address *sa)
{
	VERIFY(MUTEX_HELD(&cpq->cpq_mutex));

	if (sa->a_lun != 0) {
		return (NULL);
	}

	return (cpqary3_lookup_phys_dev_by_id(cpq, sa->a_target));
}

static int
cpqary3_read_physdevs(cpqary3_t *cpq, cpqary3_report_physical_dev_t *cprpl)
{
	cpqary3_report_physical_dev_ent_t *ents = cprpl->cprpl_data.ents;
	uint32_t count = ntohl(cprpl->cprpl_datasize) /
	    sizeof (cpqary3_report_physical_dev_ent_t);

	if (count > MAX_PHYSDEV) {
		count = MAX_PHYSDEV;
	}

	dev_err(cpq->dip, CE_NOTE, "Found %d PHYSDEVs", count);
	for (unsigned i = 0; i < count; i++) {
		cpqary3_phys_dev_t *cppd;

		mutex_enter(&cpq->cpq_mutex);
		if ((cppd = cpqary3_lookup_phys_dev_by_physid(cpq,
		    (&ents[i].cprpe_addr))) != NULL) {
			mutex_exit(&cpq->cpq_mutex);
			continue;
		}

		dev_err(cpq->dip, CE_NOTE, "NEW PHYSDEV[%u]: mode %x "
		    "tgtid %x tgt[0] %x tgt[1] %x", i,
		    ents[i].cprpe_addr.Mode,
		    ents[i].cprpe_addr.TargetId,
		    *((uint32_t *)(&(ents[i].cprpe_addr.Target[0]))),
		    *((uint32_t *)(&(ents[i].cprpe_addr.Target[1]))));

		/*
		 * This is a new Physical Dev, so add it the the list.
		 */
		if ((cppd = kmem_zalloc(sizeof (*cppd), KM_NOSLEEP)) == NULL) {
			mutex_exit(&cpq->cpq_mutex);
			return (ENOMEM);
		}

		cppd->cppd_addr = ents[i].cprpe_addr;
		bcopy(&(ents[i].cprpe_addr), &(cppd->cppd_addr),
			sizeof (PhysDevAddr_t));
		cppd->cppd_flags |= CPQARY3_PD_FLAG_WWN;

		list_create(&cppd->cppd_targets,
		    sizeof (cpqary3_target_t),
		    offsetof(cpqary3_target_t, cptg_link_phys_dev));

		cppd->cppd_ctlr = cpq;
		list_insert_tail(&cpq->cpq_phys_devs, cppd);
		mutex_exit(&cpq->cpq_mutex);
	}

	return (0);
}

static int
cpqary3_read_physdevs_ext(cpqary3_t *cpq, cpqary3_report_physical_dev_t *cprpl)
{
	cpqary3_report_physical_dev_extent_t *extents = NULL;
	uint32_t count = 0;

	extents = cprpl->cprpl_data.extents;
	count = ntohl(cprpl->cprpl_datasize) /
	    sizeof (cpqary3_report_physical_dev_extent_t);

	if (count > MAX_PHYSDEV) {
		count = MAX_PHYSDEV;
	}

	if (extents == NULL)
		return (EINVAL);

	dev_err(cpq->dip, CE_NOTE, "Found %d EXT PHYSDEVs", count);

	for (unsigned i = 0; i < count; i++) {
		cpqary3_phys_dev_t *cppd = NULL;

		if (&(extents[i]) == NULL) {
			dev_err(cpq->dip, CE_NOTE,
				"Skipping tgt index:%d, extent null...", i);
			continue;
		}

		mutex_enter(&cpq->cpq_mutex);
		if ((cppd = cpqary3_lookup_phys_dev_by_physid(cpq,
				(&(extents[i].cprpe_addr)))) != NULL) {
			/*
			 * XXX compare previous WWN with current WWN...
			 */
			mutex_exit(&cpq->cpq_mutex);
			continue;
		}

		dev_err(cpq->dip, CE_NOTE, "NEW EXT PHYSDEV[%u]: mode %x "
		    "tgtid %x tgt[0] %x tgt[1] %x", i,
		    extents[i].cprpe_addr.Mode,
		    extents[i].cprpe_addr.TargetId,
		    *((uint32_t *)(&(extents[i].cprpe_addr.Target[0]))),
		    *((uint32_t *)(&(extents[i].cprpe_addr.Target[1]))));
		dev_err(cpq->dip, CE_NOTE, "-- id %02x %02x %02x "
		    "%02x %02x %02x %02x %02x %02x %02x %02x %02x "
		    "%02x %02x %02x %02x",
		    (uint32_t)extents[i].cprpe_wwn[0],
		    (uint32_t)extents[i].cprpe_wwn[1],
		    (uint32_t)extents[i].cprpe_wwn[2],
		    (uint32_t)extents[i].cprpe_wwn[3],
		    (uint32_t)extents[i].cprpe_wwn[4],
		    (uint32_t)extents[i].cprpe_wwn[5],
		    (uint32_t)extents[i].cprpe_wwn[6],
		    (uint32_t)extents[i].cprpe_wwn[7],
		    (uint32_t)extents[i].cprpe_wwn[8],
		    (uint32_t)extents[i].cprpe_wwn[9],
		    (uint32_t)extents[i].cprpe_wwn[10],
		    (uint32_t)extents[i].cprpe_wwn[11],
		    (uint32_t)extents[i].cprpe_wwn[12],
		    (uint32_t)extents[i].cprpe_wwn[13],
		    (uint32_t)extents[i].cprpe_wwn[14],
		    (uint32_t)extents[i].cprpe_wwn[15]);

		/*
		 * This is a new Physical Device, so add it the the list.
		 */
		if ((cppd = kmem_zalloc(sizeof (*cppd), KM_NOSLEEP)) ==
		    NULL) {
			mutex_exit(&cpq->cpq_mutex);
			return (ENOMEM);
		}

		/* cppd->cppd_addr = extents[i].cprpe_addr; */
		bcopy(&(extents[i].cprpe_addr), &(cppd->cppd_addr),
			sizeof (PhysDevAddr_t));
		cppd->cppd_addr.TargetId = 0x41 + i; /* XXX: Kludge */
		/* XXX: Above kludge may not be needed, remove everywhere & test */
		bcopy(extents[i].cprpe_wwn, cppd->cppd_wwn, 16);
		cppd->cppd_flags |= CPQARY3_PD_FLAG_WWN;

		list_create(&cppd->cppd_targets,
		    sizeof (cpqary3_target_t),
		    offsetof(cpqary3_target_t, cptg_link_phys_dev));

		cppd->cppd_ctlr = cpq;
		list_insert_tail(&cpq->cpq_phys_devs, cppd);
		mutex_exit(&cpq->cpq_mutex);
	}

	return (0);
}

/*
 * Discover the currently visible set of Logical Volumes exposed by the
 * controller.
 */
int
cpqary3_discover_physical_devices(cpqary3_t *cpq, int timeout)
{
	cpqary3_command_t *cpcm;
	cpqary3_report_physical_dev_t *cprlp;
	cpqary3_report_physical_dev_req_t cprlpr = { 0 };
	int r;

	if (!ddi_in_panic()) {
		mutex_enter(&cpq->cpq_mutex);
		while (cpq->cpq_status & CPQARY3_CTLR_STATUS_DISCOVERY) {
			/*
			 * A discovery is already occuring.  Wait for
			 * completion.
			 */
			cv_wait(&cpq->cpq_cv_finishq, &cpq->cpq_mutex);
		}

		if (gethrtime() < cpq->cpq_last_discovery + 5 * NANOSEC) { /* XXX: check */
			/*
			 * A discovery completed successfully within the
			 * last five seconds.  Just use the existing data.
			 */
			mutex_exit(&cpq->cpq_mutex);
			dev_err(cpq->dip, CE_WARN,
				"discovery completed recently, skipping...");
			return (0);
		}

		cpq->cpq_status |= CPQARY3_CTLR_STATUS_DISCOVERY;
		mutex_exit(&cpq->cpq_mutex);
	}

	/*
	 * Allocate the command to send to the device, including buffer space
	 * for the returned list of Logical Volumes.
	 */
	if ((cpcm = cpqary3_command_alloc(cpq, CPQARY3_CMDTYPE_INTERNAL,
	    KM_NOSLEEP)) == NULL ||
	    cpqary3_command_attach_internal(cpq, cpcm,
	    sizeof (cpqary3_report_physical_dev_t), KM_NOSLEEP) != 0) {
		r = ENOMEM;
		mutex_enter(&cpq->cpq_mutex);
		dev_err(cpq->dip, CE_WARN, "failed allocating discover command");
		goto out;
	}

	cprlp = cpcm->cpcm_internal->cpcmi_va;

	/*
	 * According to the CISS Specification, the Report Logical LUNs
	 * command is sent to the controller itself.
	 * IMPORTANT: The Peripheral Device addressing mode is used, with LUN 0.
	 */
	cpqary3_write_lun_addr_phys(&cpcm->cpcm_va_cmd->Header.LUN,
				B_FALSE, 0, 0);

	cpcm->cpcm_va_cmd->Request.CDBLen = 12;
	cpcm->cpcm_va_cmd->Request.Timeout = timeout;
	cpcm->cpcm_va_cmd->Request.Type.Type = CISS_TYPE_CMD;
	cpcm->cpcm_va_cmd->Request.Type.Attribute = CISS_ATTR_ORDERED;
	cpcm->cpcm_va_cmd->Request.Type.Direction = CISS_XFER_READ;

	/*
	 * The Report Logical LUNs command is essentially a vendor-specific
	 * SCSI command, which we assemble into the CDB region of the command
	 * block.
	 */
	cprlpr.cprlpr_opcode = CISS_SCMD_REPORT_PHYSICAL_LUNS;
	cprlpr.cprlpr_extflag = 1; /* get wwn! */
	cprlpr.cprlpr_datasize = htonl(sizeof (cpqary3_report_physical_dev_t));
	bcopy(&cprlpr, &cpcm->cpcm_va_cmd->Request.CDB[0], 16);

	mutex_enter(&cpq->cpq_mutex);

	/*
	 * Send the command to the device.
	 */
	dev_err(cpq->dip, CE_NOTE,
			"submitting cmd: physdev discovery...");
	cpcm->cpcm_status |= CPQARY3_CMD_STATUS_POLLED;
	if (cpqary3_submit(cpq, cpcm) != 0) {
		r = EIO;
		dev_err(cpq->dip, CE_WARN,
			"failed submit cmd: physdev discovery!");
		goto out;
	}

	/*
	 * Poll for completion.
	 */
	cpcm->cpcm_expiry = gethrtime() + timeout * NANOSEC;
	if ((r = cpqary3_poll_for(cpq, cpcm)) != 0) {
		VERIFY(r == ETIMEDOUT);
		VERIFY0(cpcm->cpcm_status & CPQARY3_CMD_STATUS_POLL_COMPLETE);

		/*
		 * The command timed out; abandon it now.  Remove the POLLED
		 * flag so that the periodic routine will send an abort to
		 * clean it up next time around.
		 */
		cpcm->cpcm_status |= CPQARY3_CMD_STATUS_ABANDONED;
		cpcm->cpcm_status &= ~CPQARY3_CMD_STATUS_POLLED;
		cpcm = NULL;
	dev_err(cpq->dip, CE_WARN,
			"physdev discovery cmd timed out!");
		goto out;
	}

	if (cpcm->cpcm_status & CPQARY3_CMD_STATUS_RESET_SENT) {
		/*
		 * The controller was reset while we were trying to discover
		 * logical phys_devs.  Report failure.
		 */
		r = EIO;
		goto out;
	}

	if (cpcm->cpcm_status & CPQARY3_CMD_STATUS_ERROR) {
		ErrorInfo_t *ei = cpcm->cpcm_va_err;

		if (ei->CommandStatus != CISS_CMD_DATA_UNDERRUN) {
			dev_err(cpq->dip, CE_WARN, "physical device discovery"
			    "error: status 0x%x", ei->CommandStatus);
			r = EIO;
			goto out;
		}
	}

	if ((cprlp->cprpl_extflag & 0x1) != 0) {
		mutex_exit(&cpq->cpq_mutex);
		r = cpqary3_read_physdevs_ext(cpq, cprlp);
	} else {
		mutex_exit(&cpq->cpq_mutex);
		r = cpqary3_read_physdevs(cpq, cprlp);
	}

	mutex_enter(&cpq->cpq_mutex);
	if (r == 0) {
		/*
		 * Update the time of the last successful Logical Volume
		 * discovery:
		 */
		cpq->cpq_last_discovery = gethrtime();
	}

out:
	cpq->cpq_status &= ~CPQARY3_CTLR_STATUS_DISCOVERY;
	cv_broadcast(&cpq->cpq_cv_finishq);
	mutex_exit(&cpq->cpq_mutex);

	if (cpcm != NULL) {
		cpqary3_command_free(cpcm);
	}
	return (r);
}
