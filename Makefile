# WORKING_DIR="$(dir $(abspath $(lastword $(MAKEFILE_LIST))))"
WORKING_DIR=.
BUILD_DIR=$(WORKING_DIR)/build/

CCL=ccl64

NAME=vinoyaku
CONTENTS_DIR=$(BUILD_DIR)/$(NAME).app/Contents/
RESOURCES_DIR=$(CONTENTS_DIR)/Resources/
MACOS_DIR=$(CONTENTS_DIR)/MacOS/
NIBS=$(RESOURCES_DIR)/MainMenu.nib $(RESOURCES_DIR)/MainWindow.nib \
	$(RESOURCES_DIR)/CapturePanel.nib

build: info.plist nibs
	$(CCL) --load $(WORKING_DIR)/build.lisp -- $(MACOS_DIR)/$(NAME)

$(RESOURCES_DIR)/%.nib: $(WORKING_DIR)/rsc/%.xib
	ibtool --errors --warnings --notices --output-format human-readable-text --compile $@ $<

info.plist: prepare
	cp $(WORKING_DIR)/rsc/Info.plist $(CONTENTS_DIR)/

nibs: prepare $(NIBS)

prepare:
	mkdir -p $(RESOURCES_DIR) $(MACOS_DIR) $(CONTENTS_DIR)

clean:
	rm -rf $(BUILD_DIR)
