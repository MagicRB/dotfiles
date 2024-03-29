# MCU name
MCU = atmega32a

# Bootloader selection
BOOTLOADER = bootloadhid

# Build Options
#   change yes to no to disable
#
RAW_ENABLE = yes
#VIRTSER_ENABLE = yes        # Virtual Serial Port
BOOTMAGIC_ENABLE = no       # Enable Bootmagic Lite
MOUSEKEY_ENABLE = no        # Mouse keys
EXTRAKEY_ENABLE = no        # Audio control and System control
CONSOLE_ENABLE = no         # Console for debug
COMMAND_ENABLE = no         # Commands for debug and configuration
BACKLIGHT_ENABLE = yes      # Enable keyboard backlight functionality
RGBLIGHT_ENABLE = yes       # Enable keyboard RGB underglow
WS2812_DRIVER = i2c

# custom matrix setup
CUSTOM_MATRIX = lite
SRC = matrix.c
QUANTUM_LIB_SRC += i2c_master.c
