# pico-6502-UART: 6502 Emulator on Raspberry Pi Pico with Microsoft BASIC

This project combines a 6502 CPU emulator running on a Raspberry Pi Pico with a port of Microsoft BASIC for the 6502. The Raspberry Pi Pico emulates the 6502 CPU, RAM, ROM, and a 6522 Versatile Interface Adapter (VIA), allowing you to run 6502 assembly programs, including a full Microsoft BASIC interpreter.

## Features

*   **6502 CPU Emulation:** A software-based 6502 CPU emulator running on the Raspberry Pi Pico.
*   **RAM, ROM, and VIA Emulation:** Emulates the necessary memory and peripheral components for a functional 6502 system.
*   **PIO-based Bus Bit-Banging:** Utilizes the Pico's Programmable I/O (PIO) to accurately simulate the 6502 bus, enabling communication with external hardware or providing a robust internal emulation.
*   **Microsoft BASIC Integration:** Includes a port of Microsoft BASIC for the 6502, allowing you to program and interact with the emulated system using BASIC commands.
*   **UART Communication:** Uses UART for serial communication, providing console access. You can connect to the Pico's serial port using a terminal emulator (e.g., `minicom`, `screen`, PuTTY) at 115,200 baud 8-N-1 to interact with the emulated 6502.

## Project Structure

*   **`RAM_ROM_VIA/`**: Contains the C source code for the Raspberry Pi Pico-based 6502 emulator, including the core emulation logic, RAM/ROM/VIA emulation, and Pico SDK integration. This is where the firmware for the Pico resides.
*   **`EATERSD10/`**: This directory is related to Ben Eater's 6502 computer project.
    *   **`EATERSD10/four-bit-mode-msbasic/`**: Houses the assembly source code for the Microsoft BASIC interpreter, adapted for the 6502. It includes various configurations and build scripts for generating the BASIC ROM image.
    *   **`EATERSD10/create.py`, `EATERSD10/doit.sh`, `EATERSD10/read_binary.py`**: Scripts for ease of building, preparing, and flashing the BASIC ROM image onto the emulated system.
*   **`utilities/`**: Contains utility tools, such as `vasm` (a versatile assembler) and `bintomon` (a binary to monitor format converter), which are used in the build process for the 6502 assembly code.

## Building and Running

To build and run this project, the quickest way is to connect your pico, ensure it is in a mode to allow flashing, cd to EATERSD10 and execute ./doit.sh, else you will generally follow these steps:

1.  **Set up Raspberry Pi Pico SDK:** Ensure you have the Raspberry Pi Pico SDK installed and configured on your system. Refer to the official Pico documentation for detailed instructions.

2.  **Build the Microsoft BASIC ROM:**
    *   Navigate to the `EATERSD10/four-bit-mode-msbasic/` directory.
    *   Ensure you have `vasm` (from `utilities/vasm/`) and other necessary tools in your PATH or accessible.
    *   Use the provided `make.sh` or `doit.sh` scripts to assemble the Microsoft BASIC source code into a ROM image. For example:
        ```bash
        cd EATERSD10/four-bit-mode-msbasic
        ./make.sh eater # Or another configuration like apple, cbm, etc.
        ```
    *   This will generate the `eater.bin` (or similar) ROM image.

3.  **Integrate BASIC ROM with Emulator (if not automated):**
    *   The `EATERSD10/create.py` script may be used to convert the generated BASIC ROM binary into a format suitable for inclusion in the Pico emulator's ROM. Once the header file is created from your custom rom image, place it in the RAM_ROM_VIA folder and edit 6502emu.c to load it with #define ROM_FILE "eater.h" and #define ROM_VAR eater

4.  **Build the 6502 Emulator Firmware:**
    *   Navigate to the `RAM_ROM_VIA/` directory.
    *   Use CMake to configure and build the project:
        ```bash
        cd RAM_ROM_VIA
        mkdir build
        cd build
        cmake ..
        make
        ```
    *   This will generate the `.uf2` firmware file (e.g., `6502emu.uf2`) that can be flashed onto your Raspberry Pi Pico.

5.  **Flash the Raspberry Pi Pico:**
    *   Put your Raspberry Pi Pico into bootloader mode (hold the BOOTSEL button while plugging it into your computer).
    *   Drag and drop the generated `.uf2` file (e.g., `RAM_ROM_VIA/build/6502emu.uf2`) onto the RPI-RP2 drive that appears.

6.  **Connect and Interact:**
    *   Once flashed, the Pico will run the 6502 emulator.
    *   Connect to the Pico's UART serial port using a terminal emulator (e.g., `minicom`, `screen`, PuTTY) to interact with the emulated 6502 and the Microsoft BASIC interpreter.

## Dependencies

*   **Raspberry Pi Pico SDK:** Required for building the Pico firmware.
*   **CMake:** Build system for the Pico emulator.
*   **GNU Make:** Used by CMake for building.
*   **`vasm`:** Versatile Assembler, used for assembling the 6502 BASIC source code. (Provided in `utilities/vasm/`)
*   **Python 3:** For scripts in `EATERSD10/`.

## Contributing

Contributions are welcome! Please feel free to open issues or submit pull requests.

## License

This project is open-source. Please refer to the individual subdirectories for specific licensing information if available.
