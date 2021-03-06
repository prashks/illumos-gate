#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the
# Common Development and Distribution License (the "License").
# You may not use this file except in compliance with the License.
#
# You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
# or http://www.opensolaris.org/os/licensing.
# See the License for the specific language governing permissions
# and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each
# file and include the License file at usr/src/OPENSOLARIS.LICENSE.
# If applicable, add the following below this CDDL HEADER, with the
# fields enclosed by brackets "[]" replaced with your own identifying
# information: Portions Copyright [yyyy] [name of copyright owner]
#
# CDDL HEADER END
#
#
#ident	"%Z%%M%	%I%	%E% SMI"
#
# Copyright 2008 Sun Microsystems, Inc.  All rights reserved.
# Use is subject to license terms.
#
# uts/sparc/Makefile
#
#	This makefile drives the production of all implementation architecture
#	independent modules for the SPARC processor. (For those unsure, this
#	means the module will run on all SPARC processor based machines
#	running SunOS.)

UTSBASE = ..

include Makefile.sparc

LINT_KMODS_X1	= $(LINT_KMODS:nsmb=)
LINT_KMODS_X2	= $(LINT_KMODS_X1:smbfs=)
LINT_KMODLIBS	= $(LINT_KMODS_X2:e1000g=)
LINT_LIBS	= $(LINT_LIB) $(GEN_LINT_LIB) \
		  $(LINT_KMODLIBS:%=$(LINT_LIB_DIR)/llib-l%.ln)

$(CLOSED_BUILD)LINT_LIBS += $(CLOSED_LINT_KMODS:%=$(LINT_LIB_DIR)/llib-l%.ln)

# EXPORT DELETE START
$(CLOSED_BUILD)LINT_LIBS	+= $(SVVS_KMODS:%=$(LINT_LIB_DIR)/llib-l%.ln)
LINT_LIBS	+= $(LINT_XMODLIBS:%=$(LINT_LIB_DIR)/llib-l%.ln)
$(CLOSED_BUILD)LINT_LIBS += $(CLOSED_XMODS:%=$(LINT_LIB_DIR)/llib-l%.ln)

DRV_KMODS	+= dprov

# EXPORT DELETE END

def		:=	TARGET= def
all		:=	TARGET= all
install		:=	TARGET= install
clean		:=	TARGET= clean
clobber		:=	TARGET= clobber
lint		:=	TARGET= lint
modlintlib	:=	TARGET= modlintlib
modlist		:=	TARGET= modlist
modlist		:=	NO_STATE= -K $$MODSTATE$$$$
clean.lint	:=	TARGET= clean.lint
check		:=	TARGET= check
install_h	:=	TARGET= install_h

.KEEP_STATE:

.PARALLEL:	$(PARALLEL_KMODS) $(CLOSED_KMODS) $(SVVS) $(XMODS) \
		$(CLOSED_XMODS) config $(LINT_DEPS)

def all install clean clobber modlist: $(KMODS) $(CLOSED_KMODS) $(SVVS) \
	$(XMODS) $(CLOSED_XMODS) config

modlintlib clean.lint: $(LINT_KMODS) $(CLOSED_LINT_KMODS) $(SVVS) \
	$(XMODS) $(CLOSED_XMODS)

$(KMODS) config:	FRC
	@cd $@; pwd; $(MAKE) $(NO_STATE) $(TARGET)

$(CLOSED_KMODS):	FRC
	cd $(CLOSED)/uts/sparc/$@; pwd; $(MAKE) $(NO_STATE) $(TARGET)

$(XMODS):	FRC
	@if [ -f $@/Makefile  ]; then \
		cd $@; pwd; $(MAKE) $(NO_STATE) $(TARGET); \
	else \
		true; \
	fi

$(SVVS) $(CLOSED_XMODS):	FRC
	@if [ -f $(CLOSED)/uts/sparc/$@/Makefile  ]; then \
		cd $(CLOSED)/uts/sparc/$@; pwd; \
		    $(MAKE) $(NO_STATE) $(TARGET); \
	else \
		true; \
	fi

install_h check:	FRC
	@cd asm; pwd; $(MAKE) $(TARGET)
	@cd sys; pwd; $(MAKE) $(TARGET)
	@cd v7/sys; pwd; $(MAKE) $(TARGET)
	@cd v9/sys; pwd; $(MAKE) $(TARGET)

#
#	Full kernel lint target.
#
LINT_TARGET	= globallint

globallint:
	@-$(ECHO) "\nFULL KERNEL: global crosschecks:"
	@-$(LINT) $(LINTFLAGS) $(LINT_LIBS) 2>&1 | $(LGREP.2)

lint:	modlintlib .WAIT $(LINT_DEPS)

# EXPORT DELETE START

EXPORT_SRC:
	$(RM) Makefile+
	sed -e "/^# EXPORT DELETE START/,/^# EXPORT DELETE END/d" \
	    < Makefile > Makefile+
	$(MV) Makefile+ Makefile
	$(CHMOD) 444 Makefile

# EXPORT DELETE END

include ../Makefile.targ
