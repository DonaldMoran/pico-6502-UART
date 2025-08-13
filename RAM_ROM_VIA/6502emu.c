 /*
 * Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
 * Copyright (c) 2022 Donald Moran.
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * This software is provided under the BSD-3-Clause license:
 * - You are free to use, modify, and distribute the code.
 * - Redistributions must retain the copyright notice, this list of conditions, and the disclaimer.
 * - The names of the project and contributors may not be used to endorse or promote products without prior permission.
 * - The software is provided "as is", without warranties or liabilities.
 */


#include <stdio.h>
#include "pico/stdlib.h"
#include "pico/time.h"
#include "hardware/clocks.h"
#include "hardware/vreg.h"
#include "hardware/gpio.h"
#include "hardware/spi.h"
#include "pico/multicore.h"
#include "pico/binary_info.h"

#define CHIPS_IMPL
#include "6502.c"
#include "6522.h"

// UART1 configuration
#define tx1  8  
#define rx1  9
// UART0 configuration
#define tx0  26  
#define rx0  27

//#define BAUD_RATE 115200
#define BAUD_RATE 115200

// GPIO pin definitions
#define PIN_0 0
#define PIN_1 1
#define PIN_2 2
#define PIN_3 3
#define PIN_4 4
#define PIN_5 5
#define PIN_6 6
#define PIN_7 7
#define PIN_8 8
#define PIN_9 9
#define PIN_10 10
#define PIN_11 11
#define PIN_12 12
#define PIN_13 13
#define PIN_14 14
#define PIN_15 15
#define PIN_16 16
#define PIN_17 17
#define PIN_18 18
#define PIN_19 19
#define PIN_20 20
#define PIN_21 21
#define PIN_22 22




//#define VIA_BASE_ADDRESS UINT16_C(0x200)
#define VIA_BASE_ADDRESS UINT16_C(0x6000)
//#define ACIA_BASE UINT16_C(0x0210)
#define ACIA_BASE UINT16_C(0x5000)

// If this is active, then an overclock will be applied
#define OVERCLOCK

// ROM configuration
#define START_DELAY 0
#define ROM_START 0x8000
#define ROM_SIZE 0x8000
//#define ROM_FILE "testing.h"
//#define ROM_VAR testing
#define ROM_FILE "eater.h"
#define ROM_VAR eater
//#define ROM_FILE "wozmon.h"
//#define ROM_VAR wozmon

#ifdef VIA_BASE_ADDRESS
m6522_t via;
uint32_t gpio_dirs;
uint32_t gpio_outs;
#define GPIO_PORTA_MASK 0xFF     // PORTA of VIA is translated to GPIO pins 0 to 7
#define GPIO_PORTB_MASK 0x7F8000 // PORTB of VIA is translated to GPIO pins 8 to 15
#define GPIO_PORTA_BASE_PIN 0
#define GPIO_PORTB_BASE_PIN 15
#endif

#include ROM_FILE
#define R_VAR ROM_VAR
#define R_START ROM_START
#define R_SIZE ROM_SIZE
uint32_t old_ticks = 0;

uint8_t mem[0x10000];
absolute_time_t start;
bool running = true;
uint64_t via_pins = 0;



void via_update() {
    if ((uint32_t)(via_pins & 0XFFFFFFFF) & (uint32_t)(M6522_IRQ & 0XFFFFFFFF)) {  //if the result of this bitwise AND operation is non-zero, then it is true.
        irq6502();
    }
}

uint8_t read6502(uint16_t address) {
    if (address == 0x5000) {
        int16_t ch;
        if (uart_is_readable(uart1)) {
            ch = uart_getc(uart1);
            putchar(ch);  // Echo to USB
            //send_i2c_char(ch); // Forward output to I2C
        } else {
            ch = getchar_timeout_us(100);
        }
        if (ch != PICO_ERROR_TIMEOUT) {
            return (uint8_t)ch & 0xFF;
        }
    }
#ifdef VIA_BASE_ADDRESS
    else if ((address & 0xFFF0) == VIA_BASE_ADDRESS) {  //the range that the condition covers is 0x200 to 0x20F
        // Populate via_pins with current GPIO input states before calling m6522_tick
        uint32_t current_gpio_state = gpio_get_all();
        via_pins &= ~(M6522_PA_PINS | M6522_PB_PINS);
        via_pins |= (uint64_t)((current_gpio_state >> GPIO_PORTA_BASE_PIN) & 0xFF) << 48; // PA0-PA7 to M6522_PA0-PA7
        via_pins |= (uint64_t)((current_gpio_state >> GPIO_PORTB_BASE_PIN) & 0xFF) << 56; // PB0-PB7 to M6522_PB0-PB7

        via_pins &= ~(M6522_RS_PINS | M6522_CS2); // clear RS pins - set CS2 low
        via_pins |= (M6522_RW | M6522_CS1 | ((uint16_t)M6522_RS_PINS & address));
        via_pins = m6522_tick(&via, via_pins);
        via_update();
        return M6522_GET_DATA(via_pins);
    }
#endif
    return mem[address];
}

void write6502(uint16_t address, uint8_t value) {
    if (address == 0x5000) {
        printf("%c", value);
        uart_putc_raw(uart1, value); // Use UART1 for output
    }
#ifdef VIA_BASE_ADDRESS
    else if ((address & 0xFFF0) == VIA_BASE_ADDRESS) { // the range that the condition covers is 0x200 to 0x20F
        // Populate via_pins with current GPIO input states before calling m6522_tick
        uint32_t current_gpio_state = gpio_get_all();
        via_pins &= ~(M6522_PA_PINS | M6522_PB_PINS);
        via_pins |= (uint64_t)((current_gpio_state >> GPIO_PORTA_BASE_PIN) & 0xFF) << 48; // PA0-PA7 to M6522_PA0-PA7
        via_pins |= (uint64_t)((current_gpio_state >> GPIO_PORTB_BASE_PIN) & 0xFF) << 56; // PB0-PB7 to M6522_PB0-PB7

        via_pins &= ~(M6522_RW | M6522_RS_PINS | M6522_CS2); // SET RW pin low to write - clear data pins - clear RS pins
        via_pins |= (M6522_CS1 | ((uint16_t)M6522_RS_PINS & address));
        M6522_SET_DATA(via_pins, value);
        via_pins = m6522_tick(&via, via_pins);

        // Update GPIO directions and outputs based on VIA's internal state after write
        gpio_dirs &= ~((uint32_t)GPIO_PORTA_MASK | (uint32_t)GPIO_PORTB_MASK);
        gpio_dirs |= (uint32_t)(via.pa.ddr << GPIO_PORTA_BASE_PIN) & (uint32_t)GPIO_PORTA_MASK;
        gpio_dirs |= (uint32_t)(via.pb.ddr << GPIO_PORTB_BASE_PIN) & (uint32_t)GPIO_PORTB_MASK;
        gpio_set_dir_all_bits(gpio_dirs);

        gpio_outs &= ~((uint32_t)GPIO_PORTA_MASK | (uint32_t)GPIO_PORTB_MASK);
        gpio_outs |= (uint32_t)(via.pa.outr << GPIO_PORTA_BASE_PIN) & (uint32_t)GPIO_PORTA_MASK;
        gpio_outs |= (uint32_t)(via.pb.outr << GPIO_PORTB_BASE_PIN) & (uint32_t)GPIO_PORTB_MASK;
        gpio_put_masked(gpio_dirs, gpio_outs);

        via_update();
    }
#endif
    else {
        mem[address] = value;
    }
}

void callback() {
#ifdef VIA_BASE_ADDRESS
    for (uint16_t i = 0; i < clockticks6502 - old_ticks; i++) {
        via_pins = m6522_tick(&via, via_pins);
    }
    via_update();
    old_ticks = clockticks6502;
#endif
}

int main() {

#ifdef OVERCLOCK
    vreg_set_voltage(VREG_VOLTAGE_1_20); // Safer for 280 MHz
    sleep_ms(1000);
    set_sys_clock_khz(270000, true);
#endif

    // Initialize USB only (skip UART initialization)
    stdio_usb_init();

    // Explicitly set UART pins
    uart_init(uart0, BAUD_RATE);
    uart_init(uart1, BAUD_RATE);

    gpio_set_function(tx0, GPIO_FUNC_UART);
    gpio_set_function(rx0, GPIO_FUNC_UART);
    uart_set_hw_flow(uart0, false, false);
    uart_set_fifo_enabled(uart0, false);
    uart_set_format(uart0, 8, 1, UART_PARITY_NONE);


    gpio_set_function(tx1, GPIO_FUNC_UART);
    gpio_set_function(rx1, GPIO_FUNC_UART);
    uart_set_hw_flow(uart1, false, false);
    uart_set_fifo_enabled(uart1, false);
    uart_set_format(uart1, 8, 1, UART_PARITY_NONE);


    if (R_START + R_SIZE > 0x10000) {
        printf("Your rom will not fit. Either adjust ROM_START or ROM_SIZE\n");
        while (1) {}
    }
    for (int i = R_START; i < R_SIZE + R_START; i++) {
        mem[i] = R_VAR[i - R_START];
    }

    hookexternal(callback);
    reset6502();

#ifdef VIA_BASE_ADDRESS
    m6522_init(&via);
    m6522_reset(&via);
    gpio_dirs = GPIO_PORTB_MASK | GPIO_PORTA_MASK;
    gpio_outs = 0;
    gpio_init_mask(gpio_dirs);
    gpio_set_dir_all_bits(gpio_dirs);
#endif

    start = get_absolute_time();
    while (running) {
        step6502();
    }
    
    return 0;
}



