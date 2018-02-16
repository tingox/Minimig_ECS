# ULX3S port of Amiga emulator "Minimig"

Keyboard driver modified not to control keyboard LEDs
to avoid compatibility issues with various keyboard.

VGA to HDMI converter is replaced from "snake" template
because original video output didn't make monitor sync.
New video doesn't have hdmi-audio.

Except clock PLL generators, most (if not all) vendor specific
RAM/ROMs are converted to generic vhdl with python scripts
and other modules slightly adjusted to use new generic modules.

There is a script which unzips original archive, removes
unneed files and converts some binaries to generic vhdl.
