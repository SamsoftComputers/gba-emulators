#!/usr/bin/env python3
"""
╔════════════════════════════════════════════════════════════════════════════════════════╗
║                    COMPLETE GBA EMULATOR - PURE PYTHON EDITION                         ║
║                         Team Flames / Samsoft / Cat OS                                 ║
╠════════════════════════════════════════════════════════════════════════════════════════╣
║  ARM7TDMI CPU (ARM + Thumb) | All Video Modes | DMA | Timers | Interrupts | Sound      ║
║  16.78MHz CPU | 240x160 Display | 32KB IWRAM | 256KB EWRAM | 96KB VRAM                 ║
╚════════════════════════════════════════════════════════════════════════════════════════╝
"""

import tkinter as tk
from tkinter import filedialog, messagebox, ttk
import threading
import time
import struct
import array
import math
import os

# =============================================================================
# GBA HARDWARE CONSTANTS
# =============================================================================

# Display
SCREEN_W = 240
SCREEN_H = 160
SCALE = 3
FPS = 59.7275  # GBA actual refresh rate

# CPU Clock
CPU_FREQ = 16777216  # 16.78 MHz
CYCLES_PER_FRAME = CPU_FREQ // 60
CYCLES_PER_SCANLINE = 1232
SCANLINES_PER_FRAME = 228  # 160 visible + 68 vblank

# Memory Sizes
BIOS_SIZE = 0x4000        # 16 KB
EWRAM_SIZE = 0x40000      # 256 KB  
IWRAM_SIZE = 0x8000       # 32 KB
IO_SIZE = 0x400           # 1 KB
PALETTE_SIZE = 0x400      # 1 KB
VRAM_SIZE = 0x18000       # 96 KB
OAM_SIZE = 0x400          # 1 KB
MAX_ROM_SIZE = 0x2000000  # 32 MB

# Memory Regions
REGION_BIOS = 0x00
REGION_EWRAM = 0x02
REGION_IWRAM = 0x03
REGION_IO = 0x04
REGION_PALETTE = 0x05
REGION_VRAM = 0x06
REGION_OAM = 0x07
REGION_ROM0 = 0x08
REGION_ROM1 = 0x0A
REGION_ROM2 = 0x0C
REGION_SRAM = 0x0E

# IO Register Addresses
REG_DISPCNT = 0x000
REG_DISPSTAT = 0x004
REG_VCOUNT = 0x006
REG_BG0CNT = 0x008
REG_BG1CNT = 0x00A
REG_BG2CNT = 0x00C
REG_BG3CNT = 0x00E
REG_BG0HOFS = 0x010
REG_BG0VOFS = 0x012
REG_BG1HOFS = 0x014
REG_BG1VOFS = 0x016
REG_BG2HOFS = 0x018
REG_BG2VOFS = 0x01A
REG_BG3HOFS = 0x01C
REG_BG3VOFS = 0x01E
REG_BG2PA = 0x020
REG_BG2PB = 0x022
REG_BG2PC = 0x024
REG_BG2PD = 0x026
REG_BG2X = 0x028
REG_BG2Y = 0x02C
REG_BG3PA = 0x030
REG_BG3PB = 0x032
REG_BG3PC = 0x034
REG_BG3PD = 0x036
REG_BG3X = 0x038
REG_BG3Y = 0x03C
REG_WIN0H = 0x040
REG_WIN1H = 0x042
REG_WIN0V = 0x044
REG_WIN1V = 0x046
REG_WININ = 0x048
REG_WINOUT = 0x04A
REG_MOSAIC = 0x04C
REG_BLDCNT = 0x050
REG_BLDALPHA = 0x052
REG_BLDY = 0x054

REG_SOUND1CNT_L = 0x060
REG_SOUND1CNT_H = 0x062
REG_SOUND1CNT_X = 0x064
REG_SOUND2CNT_L = 0x068
REG_SOUND2CNT_H = 0x06C
REG_SOUND3CNT_L = 0x070
REG_SOUND3CNT_H = 0x072
REG_SOUND3CNT_X = 0x074
REG_SOUND4CNT_L = 0x078
REG_SOUND4CNT_H = 0x07C
REG_SOUNDCNT_L = 0x080
REG_SOUNDCNT_H = 0x082
REG_SOUNDCNT_X = 0x084
REG_SOUNDBIAS = 0x088
REG_WAVE_RAM = 0x090
REG_FIFO_A = 0x0A0
REG_FIFO_B = 0x0A4

REG_DMA0SAD = 0x0B0
REG_DMA0DAD = 0x0B4
REG_DMA0CNT_L = 0x0B8
REG_DMA0CNT_H = 0x0BA
REG_DMA1SAD = 0x0BC
REG_DMA1DAD = 0x0C0
REG_DMA1CNT_L = 0x0C4
REG_DMA1CNT_H = 0x0C6
REG_DMA2SAD = 0x0C8
REG_DMA2DAD = 0x0CC
REG_DMA2CNT_L = 0x0D0
REG_DMA2CNT_H = 0x0D2
REG_DMA3SAD = 0x0D4
REG_DMA3DAD = 0x0D8
REG_DMA3CNT_L = 0x0DC
REG_DMA3CNT_H = 0x0DE

REG_TM0CNT_L = 0x100
REG_TM0CNT_H = 0x102
REG_TM1CNT_L = 0x104
REG_TM1CNT_H = 0x106
REG_TM2CNT_L = 0x108
REG_TM2CNT_H = 0x10A
REG_TM3CNT_L = 0x10C
REG_TM3CNT_H = 0x10E

REG_SIODATA32 = 0x120
REG_SIOMULTI0 = 0x120
REG_SIOMULTI1 = 0x122
REG_SIOMULTI2 = 0x124
REG_SIOMULTI3 = 0x126
REG_SIOCNT = 0x128
REG_SIOMLT_SEND = 0x12A
REG_SIODATA8 = 0x12A
REG_RCNT = 0x134
REG_JOYCNT = 0x140
REG_JOY_RECV = 0x150
REG_JOY_TRANS = 0x154
REG_JOYSTAT = 0x158

REG_IE = 0x200
REG_IF = 0x202
REG_WAITCNT = 0x204
REG_IME = 0x208
REG_POSTFLG = 0x300
REG_HALTCNT = 0x301

REG_KEYINPUT = 0x130
REG_KEYCNT = 0x132

# Key Bit Masks
KEY_A = 0
KEY_B = 1
KEY_SELECT = 2
KEY_START = 3
KEY_RIGHT = 4
KEY_LEFT = 5
KEY_UP = 6
KEY_DOWN = 7
KEY_R = 8
KEY_L = 9

# Interrupt Flags
IRQ_VBLANK = 0x0001
IRQ_HBLANK = 0x0002
IRQ_VCOUNT = 0x0004
IRQ_TIMER0 = 0x0008
IRQ_TIMER1 = 0x0010
IRQ_TIMER2 = 0x0020
IRQ_TIMER3 = 0x0040
IRQ_SERIAL = 0x0080
IRQ_DMA0 = 0x0100
IRQ_DMA1 = 0x0200
IRQ_DMA2 = 0x0400
IRQ_DMA3 = 0x0800
IRQ_KEYPAD = 0x1000
IRQ_GAMEPAK = 0x2000

# CPU Modes
MODE_USR = 0x10
MODE_FIQ = 0x11
MODE_IRQ = 0x12
MODE_SVC = 0x13
MODE_ABT = 0x17
MODE_UND = 0x1B
MODE_SYS = 0x1F

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

def sign_extend(value, bits):
    """Sign extend a value from bits to 32-bit"""
    sign_bit = 1 << (bits - 1)
    return (value & (sign_bit - 1)) - (value & sign_bit)

def ror(val, r):
    """Rotate Right 32-bit"""
    val &= 0xFFFFFFFF
    r &= 31
    if r == 0:
        return val
    return ((val >> r) | (val << (32 - r))) & 0xFFFFFFFF

def lsl(val, shift):
    """Logical Shift Left"""
    if shift >= 32:
        return 0
    return (val << shift) & 0xFFFFFFFF

def lsr(val, shift):
    """Logical Shift Right"""
    if shift >= 32:
        return 0
    return (val >> shift) & 0xFFFFFFFF

def asr(val, shift):
    """Arithmetic Shift Right"""
    val &= 0xFFFFFFFF
    if shift >= 32:
        return 0xFFFFFFFF if (val & 0x80000000) else 0
    if val & 0x80000000:
        return ((val >> shift) | (0xFFFFFFFF << (32 - shift))) & 0xFFFFFFFF
    return val >> shift

def add_with_carry(a, b, carry_in=0):
    """Add with carry, return (result, carry, overflow)"""
    a &= 0xFFFFFFFF
    b &= 0xFFFFFFFF
    result = a + b + carry_in
    carry = result > 0xFFFFFFFF
    result &= 0xFFFFFFFF
    overflow = ((a ^ result) & (b ^ result) & 0x80000000) != 0
    return result, carry, overflow

def sub_with_borrow(a, b, borrow_in=0):
    """Subtract with borrow, return (result, carry, overflow)"""
    a &= 0xFFFFFFFF
    b &= 0xFFFFFFFF
    result = a - b - borrow_in
    carry = result >= 0  # No borrow occurred
    result &= 0xFFFFFFFF
    overflow = ((a ^ b) & (a ^ result) & 0x80000000) != 0
    return result, carry, overflow

def rgb555_to_rgb888(color):
    """Convert 15-bit RGB555 to 24-bit RGB888"""
    r = ((color & 0x1F) * 255) // 31
    g = (((color >> 5) & 0x1F) * 255) // 31
    b = (((color >> 10) & 0x1F) * 255) // 31
    return (r, g, b)

# =============================================================================
# MEMORY MANAGEMENT UNIT (MMU)
# =============================================================================

class MMU:
    """GBA Memory Management Unit - Handles all memory regions and I/O"""
    
    def __init__(self):
        # Memory arrays
        self.bios = bytearray(BIOS_SIZE)
        self.ewram = bytearray(EWRAM_SIZE)
        self.iwram = bytearray(IWRAM_SIZE)
        self.io = bytearray(IO_SIZE)
        self.palette = bytearray(PALETTE_SIZE)
        self.vram = bytearray(VRAM_SIZE)
        self.oam = bytearray(OAM_SIZE)
        self.rom = bytearray()
        self.sram = bytearray(0x10000)  # 64KB SRAM max
        
        # I/O State
        self.key_state = 0x03FF  # All keys released (active low)
        self.ime = 0            # Interrupt Master Enable
        self.ie = 0             # Interrupt Enable
        self.if_ = 0            # Interrupt Flags
        self.halt = False
        self.stop = False
        
        # DMA State
        self.dma = [DMAChannel(i) for i in range(4)]
        
        # Timer State
        self.timers = [Timer(i) for i in range(4)]
        
        # PPU reference (set later)
        self.ppu = None
        self.apu = None
        
        # BIOS read after bootup flag
        self.bios_read_state = 0
        
        # Waitstates
        self.wait_rom0_n = 4
        self.wait_rom0_s = 2
        self.wait_rom1_n = 4
        self.wait_rom1_s = 4
        self.wait_rom2_n = 4
        self.wait_rom2_s = 8
        self.wait_sram = 4
        
        self._init_io()
    
    def _init_io(self):
        """Initialize I/O registers to default values"""
        # DISPCNT defaults
        self.io[REG_DISPCNT] = 0x00
        self.io[REG_DISPCNT + 1] = 0x00
        # DISPSTAT
        self.io[REG_DISPSTAT] = 0x00
        self.io[REG_DISPSTAT + 1] = 0x00
        # Key input (all released)
        self.io[REG_KEYINPUT] = 0xFF
        self.io[REG_KEYINPUT + 1] = 0x03
        # SOUNDBIAS default
        self.io[REG_SOUNDBIAS] = 0x00
        self.io[REG_SOUNDBIAS + 1] = 0x02
    
    def load_bios(self, data):
        """Load BIOS ROM"""
        size = min(len(data), BIOS_SIZE)
        self.bios[:size] = data[:size]
        print(f"[MMU] Loaded BIOS: {size} bytes")
    
    def load_rom(self, data):
        """Load game ROM"""
        self.rom = bytearray(data)
        print(f"[MMU] Loaded ROM: {len(self.rom)} bytes")
        # Parse ROM header
        if len(self.rom) >= 0xC0:
            title = self.rom[0xA0:0xAC].decode('ascii', errors='ignore').strip('\x00')
            code = self.rom[0xAC:0xB0].decode('ascii', errors='ignore')
            print(f"[MMU] ROM Title: {title}, Code: {code}")
    
    def read8(self, addr):
        """Read 8-bit value from memory"""
        addr &= 0xFFFFFFFF
        region = (addr >> 24) & 0xFF
        
        if region == REGION_BIOS:
            if addr < BIOS_SIZE:
                return self.bios[addr]
            return 0
        
        elif region == REGION_EWRAM:
            return self.ewram[addr & 0x3FFFF]
        
        elif region == REGION_IWRAM:
            return self.iwram[addr & 0x7FFF]
        
        elif region == REGION_IO:
            return self._read_io(addr & 0x3FF)
        
        elif region == REGION_PALETTE:
            return self.palette[addr & 0x3FF]
        
        elif region == REGION_VRAM:
            offset = addr & 0x1FFFF
            if offset >= VRAM_SIZE:
                offset -= 0x8000
            return self.vram[offset] if offset < VRAM_SIZE else 0
        
        elif region == REGION_OAM:
            return self.oam[addr & 0x3FF]
        
        elif region in (REGION_ROM0, REGION_ROM0 + 1, REGION_ROM1, REGION_ROM1 + 1, 
                       REGION_ROM2, REGION_ROM2 + 1):
            offset = addr & 0x01FFFFFF
            if offset < len(self.rom):
                return self.rom[offset]
            return 0xFF
        
        elif region == REGION_SRAM:
            return self.sram[addr & 0xFFFF]
        
        return 0
    
    def read16(self, addr):
        """Read 16-bit value from memory (aligned)"""
        addr &= ~1
        return self.read8(addr) | (self.read8(addr + 1) << 8)
    
    def read32(self, addr):
        """Read 32-bit value from memory (aligned)"""
        addr &= ~3
        return (self.read8(addr) | 
                (self.read8(addr + 1) << 8) |
                (self.read8(addr + 2) << 16) |
                (self.read8(addr + 3) << 24))
    
    def write8(self, addr, val):
        """Write 8-bit value to memory"""
        addr &= 0xFFFFFFFF
        val &= 0xFF
        region = (addr >> 24) & 0xFF
        
        if region == REGION_BIOS:
            pass  # BIOS is read-only
        
        elif region == REGION_EWRAM:
            self.ewram[addr & 0x3FFFF] = val
        
        elif region == REGION_IWRAM:
            self.iwram[addr & 0x7FFF] = val
        
        elif region == REGION_IO:
            self._write_io(addr & 0x3FF, val)
        
        elif region == REGION_PALETTE:
            # 8-bit writes to palette write same value to both bytes
            offset = addr & 0x3FE
            self.palette[offset] = val
            self.palette[offset + 1] = val
        
        elif region == REGION_VRAM:
            offset = addr & 0x1FFFF
            if offset >= VRAM_SIZE:
                offset -= 0x8000
            if offset < VRAM_SIZE:
                # 8-bit writes to VRAM in bitmap modes only
                self.vram[offset & ~1] = val
                self.vram[(offset & ~1) + 1] = val
        
        elif region == REGION_OAM:
            pass  # 8-bit writes to OAM are ignored
        
        elif region == REGION_SRAM:
            self.sram[addr & 0xFFFF] = val
    
    def write16(self, addr, val):
        """Write 16-bit value to memory (aligned)"""
        addr &= ~1
        self.write8(addr, val & 0xFF)
        self.write8(addr + 1, (val >> 8) & 0xFF)
    
    def write32(self, addr, val):
        """Write 32-bit value to memory (aligned)"""
        addr &= ~3
        self.write8(addr, val & 0xFF)
        self.write8(addr + 1, (val >> 8) & 0xFF)
        self.write8(addr + 2, (val >> 16) & 0xFF)
        self.write8(addr + 3, (val >> 24) & 0xFF)
    
    def _read_io(self, offset):
        """Read from I/O registers"""
        if offset == REG_KEYINPUT:
            return self.key_state & 0xFF
        elif offset == REG_KEYINPUT + 1:
            return (self.key_state >> 8) & 0x03
        elif offset == REG_IE:
            return self.ie & 0xFF
        elif offset == REG_IE + 1:
            return (self.ie >> 8) & 0xFF
        elif offset == REG_IF:
            return self.if_ & 0xFF
        elif offset == REG_IF + 1:
            return (self.if_ >> 8) & 0xFF
        elif offset == REG_IME:
            return self.ime & 0xFF
        elif offset == REG_VCOUNT:
            return self.ppu.vcount if self.ppu else 0
        elif offset == REG_DISPSTAT:
            return self._get_dispstat() & 0xFF
        elif offset == REG_DISPSTAT + 1:
            return (self._get_dispstat() >> 8) & 0xFF
        
        # Timer registers
        elif REG_TM0CNT_L <= offset <= REG_TM3CNT_H + 1:
            timer_idx = (offset - REG_TM0CNT_L) // 4
            reg_offset = (offset - REG_TM0CNT_L) % 4
            if timer_idx < 4:
                if reg_offset < 2:
                    val = self.timers[timer_idx].counter
                    return val if reg_offset == 0 else (val >> 8) & 0xFF
                else:
                    return self.io[offset]
        
        # DMA registers - return stored values
        elif REG_DMA0SAD <= offset <= REG_DMA3CNT_H + 1:
            return self.io[offset]
        
        return self.io[offset] if offset < len(self.io) else 0
    
    def _write_io(self, offset, val):
        """Write to I/O registers"""
        if offset >= len(self.io):
            return
        
        old_val = self.io[offset]
        self.io[offset] = val
        
        # Handle special registers
        if offset == REG_IE:
            self.ie = (self.ie & 0xFF00) | val
        elif offset == REG_IE + 1:
            self.ie = (self.ie & 0x00FF) | (val << 8)
        
        elif offset == REG_IF:
            # Writing 1 clears the flag
            self.if_ &= ~val
        elif offset == REG_IF + 1:
            self.if_ &= ~(val << 8)
        
        elif offset == REG_IME:
            self.ime = val & 1
        
        elif offset == REG_HALTCNT:
            if val & 0x80:
                self.stop = True
            else:
                self.halt = True
        
        # Timer control
        elif offset in (REG_TM0CNT_H, REG_TM1CNT_H, REG_TM2CNT_H, REG_TM3CNT_H):
            timer_idx = (offset - REG_TM0CNT_H) // 4
            self._update_timer_control(timer_idx, val)
        
        elif offset in (REG_TM0CNT_L, REG_TM1CNT_L, REG_TM2CNT_L, REG_TM3CNT_L):
            timer_idx = (offset - REG_TM0CNT_L) // 4
            self.timers[timer_idx].reload = (self.timers[timer_idx].reload & 0xFF00) | val
        
        elif offset in (REG_TM0CNT_L + 1, REG_TM1CNT_L + 1, REG_TM2CNT_L + 1, REG_TM3CNT_L + 1):
            timer_idx = (offset - REG_TM0CNT_L - 1) // 4
            self.timers[timer_idx].reload = (self.timers[timer_idx].reload & 0x00FF) | (val << 8)
        
        # DMA control
        elif offset == REG_DMA0CNT_H + 1:
            self._update_dma_control(0)
        elif offset == REG_DMA1CNT_H + 1:
            self._update_dma_control(1)
        elif offset == REG_DMA2CNT_H + 1:
            self._update_dma_control(2)
        elif offset == REG_DMA3CNT_H + 1:
            self._update_dma_control(3)
        
        # Sound control
        elif offset == REG_SOUNDCNT_X:
            if not (val & 0x80):
                # Master sound disable - clear all sound
                for i in range(0x60, 0x90):
                    self.io[i] = 0
    
    def _get_dispstat(self):
        """Build DISPSTAT register value"""
        if not self.ppu:
            return 0
        
        stat = self.io[REG_DISPSTAT] & 0xFFF8
        
        # V-Blank flag (bit 0)
        if self.ppu.vcount >= SCREEN_H:
            stat |= 0x01
        
        # H-Blank flag (bit 1)
        if self.ppu.hblank:
            stat |= 0x02
        
        # V-Count match flag (bit 2)
        lyc = self.io[REG_DISPSTAT + 1]
        if self.ppu.vcount == lyc:
            stat |= 0x04
        
        return stat
    
    def _update_timer_control(self, idx, val):
        """Update timer control register"""
        timer = self.timers[idx]
        was_enabled = timer.enabled
        
        timer.prescaler = val & 0x03
        timer.count_up = bool(val & 0x04)
        timer.irq_enable = bool(val & 0x40)
        timer.enabled = bool(val & 0x80)
        
        # Reload counter on enable
        if timer.enabled and not was_enabled:
            timer.counter = timer.reload
        
        # Set prescaler values
        prescalers = [1, 64, 256, 1024]
        timer.prescale_val = prescalers[timer.prescaler]
    
    def _update_dma_control(self, idx):
        """Update DMA control and potentially start transfer"""
        base = REG_DMA0SAD + idx * 12
        
        sad = self.io[base] | (self.io[base+1] << 8) | (self.io[base+2] << 16) | (self.io[base+3] << 24)
        dad = self.io[base+4] | (self.io[base+5] << 8) | (self.io[base+6] << 16) | (self.io[base+7] << 24)
        cnt_l = self.io[base+8] | (self.io[base+9] << 8)
        cnt_h = self.io[base+10] | (self.io[base+11] << 8)
        
        dma = self.dma[idx]
        dma.sad = sad
        dma.dad = dad
        dma.count = cnt_l if cnt_l != 0 else (0x4000 if idx == 3 else 0x10000)
        dma.dest_ctrl = (cnt_h >> 5) & 0x03
        dma.src_ctrl = (cnt_h >> 7) & 0x03
        dma.repeat = bool(cnt_h & 0x200)
        dma.transfer_32 = bool(cnt_h & 0x400)
        dma.timing = (cnt_h >> 12) & 0x03
        dma.irq = bool(cnt_h & 0x4000)
        dma.enabled = bool(cnt_h & 0x8000)
        
        # Start immediately if timing is 0
        if dma.enabled and dma.timing == 0:
            self._run_dma(idx)
    
    def _run_dma(self, idx):
        """Execute DMA transfer"""
        dma = self.dma[idx]
        if not dma.enabled:
            return 0
        
        cycles = 0
        src = dma.sad
        dst = dma.dad
        count = dma.count
        
        src_inc = [2, -2, 0, 2] if not dma.transfer_32 else [4, -4, 0, 4]
        dst_inc = [2, -2, 0, 2] if not dma.transfer_32 else [4, -4, 0, 4]
        
        src_delta = src_inc[dma.src_ctrl]
        dst_delta = dst_inc[dma.dest_ctrl]
        
        for _ in range(count):
            if dma.transfer_32:
                val = self.read32(src)
                self.write32(dst, val)
                cycles += 4
            else:
                val = self.read16(src)
                self.write16(dst, val)
                cycles += 2
            
            src += src_delta
            dst += dst_delta
        
        # Update source/dest
        dma.sad = src
        if dma.dest_ctrl != 3:
            dma.dad = dst
        
        # Handle repeat/disable
        if not dma.repeat or dma.timing == 0:
            dma.enabled = False
            # Clear enable bit in control register
            base = REG_DMA0CNT_H + idx * 12
            self.io[base + 1] &= ~0x80
        
        # Fire IRQ if enabled
        if dma.irq:
            self.if_ |= (IRQ_DMA0 << idx)
        
        return cycles
    
    def trigger_dma(self, timing):
        """Trigger DMAs with specified timing"""
        cycles = 0
        for i in range(4):
            if self.dma[i].enabled and self.dma[i].timing == timing:
                cycles += self._run_dma(i)
        return cycles
    
    def update_timers(self, cycles):
        """Update all timers with elapsed cycles"""
        for i in range(4):
            timer = self.timers[i]
            if not timer.enabled:
                continue
            
            if timer.count_up and i > 0:
                # Cascade - handled by previous timer overflow
                continue
            
            timer.subcycles += cycles
            ticks = timer.subcycles // timer.prescale_val
            timer.subcycles %= timer.prescale_val
            
            if ticks > 0:
                self._tick_timer(i, ticks)
    
    def _tick_timer(self, idx, ticks):
        """Tick timer and handle overflow"""
        timer = self.timers[idx]
        
        new_val = timer.counter + ticks
        
        while new_val >= 0x10000:
            new_val -= 0x10000
            new_val += timer.reload
            
            # Fire IRQ
            if timer.irq_enable:
                self.if_ |= (IRQ_TIMER0 << idx)
            
            # Cascade to next timer
            if idx < 3 and self.timers[idx + 1].count_up and self.timers[idx + 1].enabled:
                self._tick_timer(idx + 1, 1)
            
            # Sound FIFO handling
            if idx == 0 or idx == 1:
                if self.apu:
                    self.apu.timer_overflow(idx)
        
        timer.counter = new_val & 0xFFFF
    
    def request_irq(self, flag):
        """Request an interrupt"""
        self.if_ |= flag
    
    def check_irq(self):
        """Check if an IRQ should be serviced"""
        return self.ime and (self.ie & self.if_) and not self.halt

# =============================================================================
# DMA CHANNEL
# =============================================================================

class DMAChannel:
    """DMA Channel state"""
    def __init__(self, idx):
        self.idx = idx
        self.sad = 0
        self.dad = 0
        self.count = 0
        self.dest_ctrl = 0
        self.src_ctrl = 0
        self.repeat = False
        self.transfer_32 = False
        self.timing = 0
        self.irq = False
        self.enabled = False

# =============================================================================
# TIMER
# =============================================================================

class Timer:
    """Timer channel state"""
    def __init__(self, idx):
        self.idx = idx
        self.counter = 0
        self.reload = 0
        self.prescaler = 0
        self.prescale_val = 1
        self.count_up = False
        self.irq_enable = False
        self.enabled = False
        self.subcycles = 0

# =============================================================================
# ARM7TDMI CPU
# =============================================================================

class ARM7TDMI:
    """ARM7TDMI CPU Core - Full ARM and Thumb instruction sets"""
    
    def __init__(self, mmu):
        self.mmu = mmu
        
        # Registers
        self.regs = [0] * 16  # R0-R15 (R13=SP, R14=LR, R15=PC)
        self.cpsr = MODE_SYS  # Current Program Status Register
        self.spsr = [0] * 6   # Saved PSRs for each mode
        
        # Banked registers
        self.regs_fiq = [0] * 7   # R8-R14 FIQ
        self.regs_svc = [0] * 2   # R13-R14 SVC
        self.regs_abt = [0] * 2   # R13-R14 ABT
        self.regs_irq = [0] * 2   # R13-R14 IRQ
        self.regs_und = [0] * 2   # R13-R14 UND
        
        # Condition flags
        self.flag_n = False  # Negative
        self.flag_z = False  # Zero
        self.flag_c = False  # Carry
        self.flag_v = False  # Overflow
        
        # State
        self.thumb = False   # Thumb mode flag
        self.irq_disable = True
        self.fiq_disable = True
        
        # Pipeline
        self.pipeline = [0, 0]
        self.pipeline_valid = [False, False]
        
        # Cycle counting
        self.cycles = 0
        
    def reset(self):
        """Reset CPU to initial state"""
        for i in range(16):
            self.regs[i] = 0
        
        self.cpsr = MODE_SYS | 0x80 | 0x40  # System mode, IRQ/FIQ disabled
        self.thumb = False
        self.irq_disable = True
        self.fiq_disable = True
        
        # Set stack pointers
        self.regs[13] = 0x03007F00  # SP
        self.regs_irq[0] = 0x03007FA0  # SP_irq
        self.regs_svc[0] = 0x03007FE0  # SP_svc
        
        # Entry point
        self.regs[15] = 0x08000000
        
        self._flush_pipeline()
        self._update_flags_from_cpsr()
    
    def _flush_pipeline(self):
        """Flush instruction pipeline"""
        self.pipeline_valid = [False, False]
    
    def _update_flags_from_cpsr(self):
        """Update flag variables from CPSR"""
        self.flag_n = bool(self.cpsr & 0x80000000)
        self.flag_z = bool(self.cpsr & 0x40000000)
        self.flag_c = bool(self.cpsr & 0x20000000)
        self.flag_v = bool(self.cpsr & 0x10000000)
        self.thumb = bool(self.cpsr & 0x20)
        self.irq_disable = bool(self.cpsr & 0x80)
        self.fiq_disable = bool(self.cpsr & 0x40)
    
    def _update_cpsr_from_flags(self):
        """Update CPSR from flag variables"""
        self.cpsr = (self.cpsr & 0x0FFFFFFF)
        if self.flag_n: self.cpsr |= 0x80000000
        if self.flag_z: self.cpsr |= 0x40000000
        if self.flag_c: self.cpsr |= 0x20000000
        if self.flag_v: self.cpsr |= 0x10000000
    
    def _set_nz(self, val):
        """Set N and Z flags based on result"""
        val &= 0xFFFFFFFF
        self.flag_n = bool(val & 0x80000000)
        self.flag_z = val == 0
    
    def _get_mode_index(self, mode):
        """Get bank index for CPU mode"""
        if mode == MODE_FIQ: return 0
        if mode == MODE_SVC: return 1
        if mode == MODE_ABT: return 2
        if mode == MODE_IRQ: return 3
        if mode == MODE_UND: return 4
        return 5  # USR/SYS
    
    def _switch_mode(self, new_mode):
        """Switch CPU mode and bank registers"""
        old_mode = self.cpsr & 0x1F
        if old_mode == new_mode:
            return
        
        # Save current registers
        if old_mode == MODE_FIQ:
            for i in range(7):
                self.regs_fiq[i] = self.regs[8 + i]
        elif old_mode in (MODE_SVC, MODE_ABT, MODE_IRQ, MODE_UND):
            bank = {MODE_SVC: self.regs_svc, MODE_ABT: self.regs_abt,
                   MODE_IRQ: self.regs_irq, MODE_UND: self.regs_und}[old_mode]
            bank[0] = self.regs[13]
            bank[1] = self.regs[14]
        
        # Load new registers
        if new_mode == MODE_FIQ:
            for i in range(7):
                self.regs[8 + i] = self.regs_fiq[i]
        elif new_mode in (MODE_SVC, MODE_ABT, MODE_IRQ, MODE_UND):
            bank = {MODE_SVC: self.regs_svc, MODE_ABT: self.regs_abt,
                   MODE_IRQ: self.regs_irq, MODE_UND: self.regs_und}[new_mode]
            self.regs[13] = bank[0]
            self.regs[14] = bank[1]
        
        self.cpsr = (self.cpsr & ~0x1F) | new_mode
    
    def check_condition(self, cond):
        """Check ARM condition code"""
        if cond == 0x0: return self.flag_z                    # EQ
        if cond == 0x1: return not self.flag_z                # NE
        if cond == 0x2: return self.flag_c                    # CS/HS
        if cond == 0x3: return not self.flag_c                # CC/LO
        if cond == 0x4: return self.flag_n                    # MI
        if cond == 0x5: return not self.flag_n                # PL
        if cond == 0x6: return self.flag_v                    # VS
        if cond == 0x7: return not self.flag_v                # VC
        if cond == 0x8: return self.flag_c and not self.flag_z  # HI
        if cond == 0x9: return not self.flag_c or self.flag_z   # LS
        if cond == 0xA: return self.flag_n == self.flag_v       # GE
        if cond == 0xB: return self.flag_n != self.flag_v       # LT
        if cond == 0xC: return not self.flag_z and (self.flag_n == self.flag_v)  # GT
        if cond == 0xD: return self.flag_z or (self.flag_n != self.flag_v)       # LE
        if cond == 0xE: return True                           # AL
        return False
    
    def service_irq(self):
        """Service an IRQ"""
        if self.irq_disable:
            return False
        
        # Save CPSR
        mode_idx = self._get_mode_index(MODE_IRQ)
        self.spsr[mode_idx] = self.cpsr
        
        # Switch to IRQ mode
        self._switch_mode(MODE_IRQ)
        
        # Save return address
        self.regs[14] = self.regs[15] - (2 if self.thumb else 4) + 4
        
        # Jump to IRQ vector
        self.regs[15] = 0x00000018
        
        # Disable IRQs, switch to ARM
        self.cpsr |= 0x80
        self.cpsr &= ~0x20
        self.irq_disable = True
        self.thumb = False
        
        self._flush_pipeline()
        return True
    
    def step(self):
        """Execute one instruction"""
        # Check for halt
        if self.mmu.halt:
            if self.mmu.ie & self.mmu.if_:
                self.mmu.halt = False
            else:
                self.cycles += 1
                return 1
        
        # Check for IRQ
        if not self.irq_disable and (self.mmu.ime and (self.mmu.ie & self.mmu.if_)):
            self.service_irq()
        
        pc = self.regs[15]
        
        if self.thumb:
            op = self.mmu.read16(pc)
            self.regs[15] = (pc + 2) & 0xFFFFFFFF
            cycles = self._exec_thumb(op)
        else:
            op = self.mmu.read32(pc)
            self.regs[15] = (pc + 4) & 0xFFFFFFFF
            cycles = self._exec_arm(op)
        
        self.cycles += cycles
        return cycles
    
    # =========================================================================
    # ARM INSTRUCTION EXECUTION
    # =========================================================================
    
    def _exec_arm(self, op):
        """Execute ARM instruction"""
        cond = (op >> 28) & 0xF
        if not self.check_condition(cond):
            return 1
        
        # Decode instruction type
        bits_27_25 = (op >> 25) & 0x7
        bit_4 = (op >> 4) & 1
        bit_7 = (op >> 7) & 1
        
        # Branch / Branch with Link
        if bits_27_25 == 0b101:
            return self._arm_branch(op)
        
        # Branch and Exchange (BX)
        if (op & 0x0FFFFFF0) == 0x012FFF10:
            return self._arm_bx(op)
        
        # Software Interrupt
        if bits_27_25 == 0b111 and (op & 0x01000000):
            return self._arm_swi(op)
        
        # Multiply / Multiply Long
        if bits_27_25 == 0b000 and (op & 0x90) == 0x90:
            if bit_7 and not bit_4:
                # Halfword/Signed transfer
                return self._arm_halfword_transfer(op)
            else:
                return self._arm_multiply(op)
        
        # Single Data Swap
        if (op & 0x0FB00FF0) == 0x01000090:
            return self._arm_swap(op)
        
        # Single Data Transfer (LDR/STR)
        if bits_27_25 in (0b010, 0b011):
            return self._arm_single_transfer(op)
        
        # Block Data Transfer (LDM/STM)
        if bits_27_25 == 0b100:
            return self._arm_block_transfer(op)
        
        # Data Processing / PSR Transfer
        if bits_27_25 in (0b000, 0b001):
            if (op & 0x0FBF0FFF) == 0x010F0000:
                return self._arm_mrs(op)
            elif (op & 0x0DB0F000) == 0x0120F000:
                return self._arm_msr(op)
            else:
                return self._arm_data_processing(op)
        
        # Coprocessor (not used on GBA)
        
        return 1  # Unknown instruction
    
    def _arm_branch(self, op):
        """ARM Branch / Branch with Link"""
        link = bool(op & 0x01000000)
        offset = op & 0x00FFFFFF
        if offset & 0x00800000:
            offset |= 0xFF000000  # Sign extend
        offset = (offset << 2) & 0xFFFFFFFF
        if offset & 0x80000000:
            offset = offset - 0x100000000
        
        if link:
            self.regs[14] = (self.regs[15] - 4) & 0xFFFFFFFF
        
        self.regs[15] = (self.regs[15] + offset) & 0xFFFFFFFF
        self._flush_pipeline()
        return 3
    
    def _arm_bx(self, op):
        """ARM Branch and Exchange"""
        rn = op & 0xF
        addr = self.regs[rn]
        
        self.thumb = bool(addr & 1)
        self.cpsr = (self.cpsr & ~0x20) | (0x20 if self.thumb else 0)
        
        self.regs[15] = addr & (~1 if self.thumb else ~3)
        self._flush_pipeline()
        return 3
    
    def _arm_swi(self, op):
        """ARM Software Interrupt"""
        # Save CPSR
        mode_idx = self._get_mode_index(MODE_SVC)
        self.spsr[mode_idx] = self.cpsr
        
        # Switch to SVC mode
        self._switch_mode(MODE_SVC)
        
        # Save return address
        self.regs[14] = (self.regs[15] - 4) & 0xFFFFFFFF
        
        # Jump to SWI vector
        self.regs[15] = 0x00000008
        
        # Disable IRQs, switch to ARM
        self.cpsr |= 0x80
        self.cpsr &= ~0x20
        self.irq_disable = True
        self.thumb = False
        
        self._flush_pipeline()
        return 3
    
    def _arm_multiply(self, op):
        """ARM Multiply instructions"""
        accumulate = bool(op & 0x00200000)
        set_flags = bool(op & 0x00100000)
        rd = (op >> 16) & 0xF
        rn = (op >> 12) & 0xF
        rs = (op >> 8) & 0xF
        rm = op & 0xF
        
        # Check for long multiply
        if op & 0x00800000:
            # MULL/MLAL
            signed = bool(op & 0x00400000)
            rdhi = (op >> 16) & 0xF
            rdlo = (op >> 12) & 0xF
            
            a = self.regs[rm]
            b = self.regs[rs]
            
            if signed:
                if a & 0x80000000: a = a - 0x100000000
                if b & 0x80000000: b = b - 0x100000000
            
            result = a * b
            
            if accumulate:
                acc = (self.regs[rdhi] << 32) | self.regs[rdlo]
                result += acc
            
            result &= 0xFFFFFFFFFFFFFFFF
            
            self.regs[rdlo] = result & 0xFFFFFFFF
            self.regs[rdhi] = (result >> 32) & 0xFFFFFFFF
            
            if set_flags:
                self.flag_n = bool(result & 0x8000000000000000)
                self.flag_z = result == 0
            
            return 4
        else:
            # MUL/MLA
            result = (self.regs[rm] * self.regs[rs]) & 0xFFFFFFFF
            
            if accumulate:
                result = (result + self.regs[rn]) & 0xFFFFFFFF
            
            self.regs[rd] = result
            
            if set_flags:
                self._set_nz(result)
            
            return 3
    
    def _arm_swap(self, op):
        """ARM Single Data Swap"""
        byte = bool(op & 0x00400000)
        rn = (op >> 16) & 0xF
        rd = (op >> 12) & 0xF
        rm = op & 0xF
        
        addr = self.regs[rn]
        
        if byte:
            tmp = self.mmu.read8(addr)
            self.mmu.write8(addr, self.regs[rm] & 0xFF)
            self.regs[rd] = tmp
        else:
            tmp = self.mmu.read32(addr)
            self.mmu.write32(addr, self.regs[rm])
            self.regs[rd] = tmp
        
        return 4
    
    def _arm_halfword_transfer(self, op):
        """ARM Halfword and Signed Data Transfer"""
        pre = bool(op & 0x01000000)
        up = bool(op & 0x00800000)
        imm = bool(op & 0x00400000)
        write_back = bool(op & 0x00200000)
        load = bool(op & 0x00100000)
        rn = (op >> 16) & 0xF
        rd = (op >> 12) & 0xF
        sh = (op >> 5) & 0x3
        rm = op & 0xF
        
        if imm:
            offset = ((op >> 4) & 0xF0) | (op & 0xF)
        else:
            offset = self.regs[rm]
        
        addr = self.regs[rn]
        
        if pre:
            addr = (addr + offset) if up else (addr - offset)
            addr &= 0xFFFFFFFF
        
        if load:
            if sh == 1:  # LDRH
                val = self.mmu.read16(addr)
            elif sh == 2:  # LDRSB
                val = self.mmu.read8(addr)
                if val & 0x80:
                    val |= 0xFFFFFF00
            elif sh == 3:  # LDRSH
                val = self.mmu.read16(addr)
                if val & 0x8000:
                    val |= 0xFFFF0000
            else:
                val = 0
            self.regs[rd] = val & 0xFFFFFFFF
        else:
            if sh == 1:  # STRH
                self.mmu.write16(addr, self.regs[rd] & 0xFFFF)
        
        if not pre:
            addr = self.regs[rn]
            addr = (addr + offset) if up else (addr - offset)
            addr &= 0xFFFFFFFF
        
        if write_back or not pre:
            self.regs[rn] = addr
        
        return 3 if load else 2
    
    def _arm_single_transfer(self, op):
        """ARM Single Data Transfer (LDR/STR)"""
        imm = not bool(op & 0x02000000)
        pre = bool(op & 0x01000000)
        up = bool(op & 0x00800000)
        byte = bool(op & 0x00400000)
        write_back = bool(op & 0x00200000)
        load = bool(op & 0x00100000)
        rn = (op >> 16) & 0xF
        rd = (op >> 12) & 0xF
        
        if imm:
            offset = op & 0xFFF
        else:
            rm = op & 0xF
            shift_type = (op >> 5) & 0x3
            shift_amt = (op >> 7) & 0x1F
            offset = self._barrel_shift(self.regs[rm], shift_type, shift_amt, False)[0]
        
        addr = self.regs[rn]
        if rn == 15:
            addr += 8  # PC offset
        
        if pre:
            addr = (addr + offset) if up else (addr - offset)
            addr &= 0xFFFFFFFF
        
        if load:
            if byte:
                val = self.mmu.read8(addr)
            else:
                val = self.mmu.read32(addr)
                # Handle misaligned reads
                if addr & 3:
                    rotate = (addr & 3) * 8
                    val = ror(val, rotate)
            
            if rd == 15:
                self.regs[15] = val & ~3
                self._flush_pipeline()
            else:
                self.regs[rd] = val
        else:
            val = self.regs[rd]
            if rd == 15:
                val += 4  # Store PC+12
            
            if byte:
                self.mmu.write8(addr, val & 0xFF)
            else:
                self.mmu.write32(addr, val)
        
        if not pre:
            addr = self.regs[rn]
            addr = (addr + offset) if up else (addr - offset)
            addr &= 0xFFFFFFFF
        
        if (write_back or not pre) and (not load or rd != rn):
            self.regs[rn] = addr
        
        return 3 if load else 2
    
    def _arm_block_transfer(self, op):
        """ARM Block Data Transfer (LDM/STM)"""
        pre = bool(op & 0x01000000)
        up = bool(op & 0x00800000)
        psr = bool(op & 0x00400000)
        write_back = bool(op & 0x00200000)
        load = bool(op & 0x00100000)
        rn = (op >> 16) & 0xF
        rlist = op & 0xFFFF
        
        addr = self.regs[rn]
        
        # Count registers
        count = bin(rlist).count('1')
        if count == 0:
            return 1
        
        # Calculate start address
        if up:
            if pre:
                addr += 4
        else:
            addr -= count * 4
            if not pre:
                addr += 4
        
        addr &= 0xFFFFFFFF
        first_addr = addr
        
        cycles = 2
        
        for i in range(16):
            if rlist & (1 << i):
                if load:
                    val = self.mmu.read32(addr)
                    if psr and (rlist & 0x8000):
                        # Load with S bit and R15 - restore CPSR
                        mode = self.cpsr & 0x1F
                        if mode != MODE_USR and mode != MODE_SYS:
                            self.cpsr = self.spsr[self._get_mode_index(mode)]
                            self._update_flags_from_cpsr()
                    
                    if i == 15:
                        self.regs[15] = val & ~3
                        self._flush_pipeline()
                    else:
                        self.regs[i] = val
                else:
                    val = self.regs[i]
                    if i == 15:
                        val += 4
                    self.mmu.write32(addr, val)
                
                addr = (addr + 4) & 0xFFFFFFFF
                cycles += 1
        
        if write_back:
            if up:
                self.regs[rn] = (first_addr + count * 4 - (4 if pre else 0)) & 0xFFFFFFFF
            else:
                self.regs[rn] = (first_addr - (4 if not pre else 0)) & 0xFFFFFFFF
        
        return cycles
    
    def _arm_mrs(self, op):
        """ARM MRS - Move PSR to Register"""
        use_spsr = bool(op & 0x00400000)
        rd = (op >> 12) & 0xF
        
        if use_spsr:
            mode = self.cpsr & 0x1F
            self.regs[rd] = self.spsr[self._get_mode_index(mode)]
        else:
            self._update_cpsr_from_flags()
            self.regs[rd] = self.cpsr
        
        return 1
    
    def _arm_msr(self, op):
        """ARM MSR - Move Register to PSR"""
        imm = bool(op & 0x02000000)
        use_spsr = bool(op & 0x00400000)
        
        # Field mask
        mask = 0
        if op & 0x00010000: mask |= 0x000000FF  # Control
        if op & 0x00020000: mask |= 0x0000FF00  # Extension
        if op & 0x00040000: mask |= 0x00FF0000  # Status
        if op & 0x00080000: mask |= 0xFF000000  # Flags
        
        if imm:
            val = op & 0xFF
            rotate = ((op >> 8) & 0xF) * 2
            val = ror(val, rotate)
        else:
            rm = op & 0xF
            val = self.regs[rm]
        
        if use_spsr:
            mode = self.cpsr & 0x1F
            idx = self._get_mode_index(mode)
            self.spsr[idx] = (self.spsr[idx] & ~mask) | (val & mask)
        else:
            # Only privileged modes can change control bits
            mode = self.cpsr & 0x1F
            if mode == MODE_USR:
                mask &= 0xF0000000  # Only flags
            
            self.cpsr = (self.cpsr & ~mask) | (val & mask)
            self._update_flags_from_cpsr()
            
            # Handle mode change
            if mask & 0x1F:
                new_mode = self.cpsr & 0x1F
                if new_mode != mode:
                    self._switch_mode(new_mode)
        
        return 1
    
    def _arm_data_processing(self, op):
        """ARM Data Processing instructions"""
        imm = bool(op & 0x02000000)
        opcode = (op >> 21) & 0xF
        set_flags = bool(op & 0x00100000)
        rn = (op >> 16) & 0xF
        rd = (op >> 12) & 0xF
        
        # Get operand 2
        if imm:
            val = op & 0xFF
            rotate = ((op >> 8) & 0xF) * 2
            op2 = ror(val, rotate)
            shift_carry = self.flag_c if rotate == 0 else bool(op2 & 0x80000000)
        else:
            rm = op & 0xF
            shift_type = (op >> 5) & 0x3
            
            if op & 0x10:  # Register shift
                rs = (op >> 8) & 0xF
                shift_amt = self.regs[rs] & 0xFF
            else:  # Immediate shift
                shift_amt = (op >> 7) & 0x1F
            
            op2, shift_carry = self._barrel_shift(self.regs[rm], shift_type, shift_amt, not (op & 0x10))
        
        op1 = self.regs[rn]
        if rn == 15:
            op1 += 8 if not (op & 0x10) else 12
        
        result = 0
        write_result = True
        carry = self.flag_c
        overflow = self.flag_v
        
        if opcode == 0x0:  # AND
            result = op1 & op2
            carry = shift_carry
        elif opcode == 0x1:  # EOR
            result = op1 ^ op2
            carry = shift_carry
        elif opcode == 0x2:  # SUB
            result, carry, overflow = sub_with_borrow(op1, op2)
        elif opcode == 0x3:  # RSB
            result, carry, overflow = sub_with_borrow(op2, op1)
        elif opcode == 0x4:  # ADD
            result, carry, overflow = add_with_carry(op1, op2)
        elif opcode == 0x5:  # ADC
            result, carry, overflow = add_with_carry(op1, op2, 1 if self.flag_c else 0)
        elif opcode == 0x6:  # SBC
            result, carry, overflow = sub_with_borrow(op1, op2, 0 if self.flag_c else 1)
        elif opcode == 0x7:  # RSC
            result, carry, overflow = sub_with_borrow(op2, op1, 0 if self.flag_c else 1)
        elif opcode == 0x8:  # TST
            result = op1 & op2
            carry = shift_carry
            write_result = False
        elif opcode == 0x9:  # TEQ
            result = op1 ^ op2
            carry = shift_carry
            write_result = False
        elif opcode == 0xA:  # CMP
            result, carry, overflow = sub_with_borrow(op1, op2)
            write_result = False
        elif opcode == 0xB:  # CMN
            result, carry, overflow = add_with_carry(op1, op2)
            write_result = False
        elif opcode == 0xC:  # ORR
            result = op1 | op2
            carry = shift_carry
        elif opcode == 0xD:  # MOV
            result = op2
            carry = shift_carry
        elif opcode == 0xE:  # BIC
            result = op1 & ~op2
            carry = shift_carry
        elif opcode == 0xF:  # MVN
            result = ~op2 & 0xFFFFFFFF
            carry = shift_carry
        
        result &= 0xFFFFFFFF
        
        if write_result:
            if rd == 15:
                if set_flags:
                    # Restore CPSR from SPSR
                    mode = self.cpsr & 0x1F
                    if mode != MODE_USR and mode != MODE_SYS:
                        self.cpsr = self.spsr[self._get_mode_index(mode)]
                        self._update_flags_from_cpsr()
                
                self.regs[15] = result & ~3
                self._flush_pipeline()
            else:
                self.regs[rd] = result
        
        if set_flags and (rd != 15 or not write_result):
            self._set_nz(result)
            self.flag_c = carry
            self.flag_v = overflow
        
        return 1
    
    def _barrel_shift(self, val, shift_type, shift_amt, imm_shift):
        """Perform barrel shifter operation"""
        val &= 0xFFFFFFFF
        carry = self.flag_c
        
        if shift_type == 0:  # LSL
            if shift_amt == 0:
                result = val
            elif shift_amt < 32:
                carry = bool(val & (1 << (32 - shift_amt)))
                result = (val << shift_amt) & 0xFFFFFFFF
            elif shift_amt == 32:
                carry = bool(val & 1)
                result = 0
            else:
                carry = False
                result = 0
        
        elif shift_type == 1:  # LSR
            if shift_amt == 0:
                if imm_shift:
                    carry = bool(val & 0x80000000)
                    result = 0
                else:
                    result = val
            elif shift_amt < 32:
                carry = bool(val & (1 << (shift_amt - 1)))
                result = val >> shift_amt
            elif shift_amt == 32:
                carry = bool(val & 0x80000000)
                result = 0
            else:
                carry = False
                result = 0
        
        elif shift_type == 2:  # ASR
            if shift_amt == 0:
                if imm_shift:
                    carry = bool(val & 0x80000000)
                    result = 0xFFFFFFFF if carry else 0
                else:
                    result = val
            elif shift_amt < 32:
                carry = bool(val & (1 << (shift_amt - 1)))
                result = asr(val, shift_amt)
            else:
                carry = bool(val & 0x80000000)
                result = 0xFFFFFFFF if carry else 0
        
        elif shift_type == 3:  # ROR
            if shift_amt == 0:
                if imm_shift:
                    # RRX
                    carry_in = 1 if self.flag_c else 0
                    carry = bool(val & 1)
                    result = (carry_in << 31) | (val >> 1)
                else:
                    result = val
            else:
                shift_amt &= 31
                if shift_amt == 0:
                    carry = bool(val & 0x80000000)
                    result = val
                else:
                    carry = bool(val & (1 << (shift_amt - 1)))
                    result = ror(val, shift_amt)
        
        return result, carry
    
    # =========================================================================
    # THUMB INSTRUCTION EXECUTION
    # =========================================================================
    
    def _exec_thumb(self, op):
        """Execute Thumb instruction"""
        # Decode based on top bits
        top = (op >> 13) & 0x7
        
        if top == 0b000:
            if (op >> 11) & 0x3 == 0b11:
                return self._thumb_add_sub(op)
            else:
                return self._thumb_shift(op)
        
        elif top == 0b001:
            return self._thumb_imm(op)
        
        elif top == 0b010:
            if (op >> 10) & 0x7 == 0b000:
                return self._thumb_alu(op)
            elif (op >> 10) & 0x3 == 0b01:
                return self._thumb_hireg(op)
            elif (op >> 11) & 0x1 == 0b1:
                return self._thumb_pc_load(op)
            else:
                return self._thumb_load_store_reg(op)
        
        elif top == 0b011:
            return self._thumb_load_store_imm(op)
        
        elif top == 0b100:
            if (op >> 12) & 0x1 == 0:
                return self._thumb_load_store_half(op)
            else:
                return self._thumb_sp_load_store(op)
        
        elif top == 0b101:
            if (op >> 12) & 0x1 == 0:
                return self._thumb_load_addr(op)
            else:
                return self._thumb_sp_add(op)
        
        elif top == 0b110:
            if (op >> 12) & 0x1 == 0:
                return self._thumb_push_pop(op)
            else:
                if (op >> 8) & 0xF == 0xF:
                    return self._thumb_swi(op)
                else:
                    return self._thumb_cond_branch(op)
        
        elif top == 0b111:
            if (op >> 11) & 0x3 == 0b00:
                return self._thumb_uncond_branch(op)
            else:
                return self._thumb_long_branch(op)
        
        return 1
    
    def _thumb_shift(self, op):
        """Thumb Move Shifted Register"""
        opcode = (op >> 11) & 0x3
        offset = (op >> 6) & 0x1F
        rs = (op >> 3) & 0x7
        rd = op & 0x7
        
        val = self.regs[rs]
        
        if opcode == 0:  # LSL
            if offset == 0:
                result = val
            else:
                self.flag_c = bool(val & (1 << (32 - offset)))
                result = (val << offset) & 0xFFFFFFFF
        elif opcode == 1:  # LSR
            if offset == 0:
                self.flag_c = bool(val & 0x80000000)
                result = 0
            else:
                self.flag_c = bool(val & (1 << (offset - 1)))
                result = val >> offset
        elif opcode == 2:  # ASR
            if offset == 0:
                self.flag_c = bool(val & 0x80000000)
                result = 0xFFFFFFFF if self.flag_c else 0
            else:
                self.flag_c = bool(val & (1 << (offset - 1)))
                result = asr(val, offset)
        
        self.regs[rd] = result
        self._set_nz(result)
        return 1
    
    def _thumb_add_sub(self, op):
        """Thumb Add/Subtract"""
        imm = bool(op & 0x400)
        sub = bool(op & 0x200)
        rn_off = (op >> 6) & 0x7
        rs = (op >> 3) & 0x7
        rd = op & 0x7
        
        op1 = self.regs[rs]
        op2 = rn_off if imm else self.regs[rn_off]
        
        if sub:
            result, self.flag_c, self.flag_v = sub_with_borrow(op1, op2)
        else:
            result, self.flag_c, self.flag_v = add_with_carry(op1, op2)
        
        self.regs[rd] = result
        self._set_nz(result)
        return 1
    
    def _thumb_imm(self, op):
        """Thumb Move/Compare/Add/Subtract Immediate"""
        opcode = (op >> 11) & 0x3
        rd = (op >> 8) & 0x7
        offset = op & 0xFF
        
        if opcode == 0:  # MOV
            self.regs[rd] = offset
            self._set_nz(offset)
        elif opcode == 1:  # CMP
            result, self.flag_c, self.flag_v = sub_with_borrow(self.regs[rd], offset)
            self._set_nz(result)
        elif opcode == 2:  # ADD
            result, self.flag_c, self.flag_v = add_with_carry(self.regs[rd], offset)
            self.regs[rd] = result
            self._set_nz(result)
        elif opcode == 3:  # SUB
            result, self.flag_c, self.flag_v = sub_with_borrow(self.regs[rd], offset)
            self.regs[rd] = result
            self._set_nz(result)
        
        return 1
    
    def _thumb_alu(self, op):
        """Thumb ALU Operations"""
        opcode = (op >> 6) & 0xF
        rs = (op >> 3) & 0x7
        rd = op & 0x7
        
        op1 = self.regs[rd]
        op2 = self.regs[rs]
        result = 0
        
        if opcode == 0x0:  # AND
            result = op1 & op2
        elif opcode == 0x1:  # EOR
            result = op1 ^ op2
        elif opcode == 0x2:  # LSL
            shift = op2 & 0xFF
            if shift == 0:
                result = op1
            elif shift < 32:
                self.flag_c = bool(op1 & (1 << (32 - shift)))
                result = (op1 << shift) & 0xFFFFFFFF
            elif shift == 32:
                self.flag_c = bool(op1 & 1)
                result = 0
            else:
                self.flag_c = False
                result = 0
        elif opcode == 0x3:  # LSR
            shift = op2 & 0xFF
            if shift == 0:
                result = op1
            elif shift < 32:
                self.flag_c = bool(op1 & (1 << (shift - 1)))
                result = op1 >> shift
            elif shift == 32:
                self.flag_c = bool(op1 & 0x80000000)
                result = 0
            else:
                self.flag_c = False
                result = 0
        elif opcode == 0x4:  # ASR
            shift = op2 & 0xFF
            if shift == 0:
                result = op1
            elif shift < 32:
                self.flag_c = bool(op1 & (1 << (shift - 1)))
                result = asr(op1, shift)
            else:
                self.flag_c = bool(op1 & 0x80000000)
                result = 0xFFFFFFFF if self.flag_c else 0
        elif opcode == 0x5:  # ADC
            result, self.flag_c, self.flag_v = add_with_carry(op1, op2, 1 if self.flag_c else 0)
        elif opcode == 0x6:  # SBC
            result, self.flag_c, self.flag_v = sub_with_borrow(op1, op2, 0 if self.flag_c else 1)
        elif opcode == 0x7:  # ROR
            shift = op2 & 0xFF
            if shift == 0:
                result = op1
            else:
                shift &= 31
                if shift == 0:
                    self.flag_c = bool(op1 & 0x80000000)
                    result = op1
                else:
                    self.flag_c = bool(op1 & (1 << (shift - 1)))
                    result = ror(op1, shift)
        elif opcode == 0x8:  # TST
            result = op1 & op2
            self._set_nz(result)
            return 1
        elif opcode == 0x9:  # NEG
            result, self.flag_c, self.flag_v = sub_with_borrow(0, op2)
        elif opcode == 0xA:  # CMP
            result, self.flag_c, self.flag_v = sub_with_borrow(op1, op2)
            self._set_nz(result)
            return 1
        elif opcode == 0xB:  # CMN
            result, self.flag_c, self.flag_v = add_with_carry(op1, op2)
            self._set_nz(result)
            return 1
        elif opcode == 0xC:  # ORR
            result = op1 | op2
        elif opcode == 0xD:  # MUL
            result = (op1 * op2) & 0xFFFFFFFF
        elif opcode == 0xE:  # BIC
            result = op1 & ~op2
        elif opcode == 0xF:  # MVN
            result = ~op2 & 0xFFFFFFFF
        
        self.regs[rd] = result
        self._set_nz(result)
        return 1
    
    def _thumb_hireg(self, op):
        """Thumb Hi Register Operations / BX"""
        opcode = (op >> 8) & 0x3
        h1 = (op >> 7) & 0x1
        h2 = (op >> 6) & 0x1
        rs = ((op >> 3) & 0x7) | (h2 << 3)
        rd = (op & 0x7) | (h1 << 3)
        
        op1 = self.regs[rd]
        op2 = self.regs[rs]
        
        if opcode == 0:  # ADD
            result = (op1 + op2) & 0xFFFFFFFF
            if rd == 15:
                self.regs[15] = result & ~1
                self._flush_pipeline()
            else:
                self.regs[rd] = result
        elif opcode == 1:  # CMP
            result, self.flag_c, self.flag_v = sub_with_borrow(op1, op2)
            self._set_nz(result)
        elif opcode == 2:  # MOV
            if rd == 15:
                self.regs[15] = op2 & ~1
                self._flush_pipeline()
            else:
                self.regs[rd] = op2
        elif opcode == 3:  # BX
            self.thumb = bool(op2 & 1)
            self.cpsr = (self.cpsr & ~0x20) | (0x20 if self.thumb else 0)
            self.regs[15] = op2 & (~1 if self.thumb else ~3)
            self._flush_pipeline()
        
        return 1
    
    def _thumb_pc_load(self, op):
        """Thumb PC-relative Load"""
        rd = (op >> 8) & 0x7
        offset = (op & 0xFF) << 2
        
        addr = ((self.regs[15] + 2) & ~2) + offset
        self.regs[rd] = self.mmu.read32(addr)
        return 3
    
    def _thumb_load_store_reg(self, op):
        """Thumb Load/Store with Register Offset"""
        opcode = (op >> 10) & 0x3
        ro = (op >> 6) & 0x7
        rb = (op >> 3) & 0x7
        rd = op & 0x7
        
        addr = (self.regs[rb] + self.regs[ro]) & 0xFFFFFFFF
        
        if opcode == 0:  # STR
            self.mmu.write32(addr, self.regs[rd])
        elif opcode == 1:  # STRB
            self.mmu.write8(addr, self.regs[rd] & 0xFF)
        elif opcode == 2:  # LDR
            val = self.mmu.read32(addr)
            if addr & 3:
                val = ror(val, (addr & 3) * 8)
            self.regs[rd] = val
        elif opcode == 3:  # LDRB
            self.regs[rd] = self.mmu.read8(addr)
        
        return 3 if opcode >= 2 else 2
    
    def _thumb_load_store_imm(self, op):
        """Thumb Load/Store with Immediate Offset"""
        byte = bool(op & 0x1000)
        load = bool(op & 0x0800)
        offset = ((op >> 6) & 0x1F)
        rb = (op >> 3) & 0x7
        rd = op & 0x7
        
        if not byte:
            offset <<= 2
        
        addr = (self.regs[rb] + offset) & 0xFFFFFFFF
        
        if load:
            if byte:
                self.regs[rd] = self.mmu.read8(addr)
            else:
                val = self.mmu.read32(addr)
                if addr & 3:
                    val = ror(val, (addr & 3) * 8)
                self.regs[rd] = val
            return 3
        else:
            if byte:
                self.mmu.write8(addr, self.regs[rd] & 0xFF)
            else:
                self.mmu.write32(addr, self.regs[rd])
            return 2
    
    def _thumb_load_store_half(self, op):
        """Thumb Load/Store Halfword"""
        load = bool(op & 0x0800)
        offset = ((op >> 6) & 0x1F) << 1
        rb = (op >> 3) & 0x7
        rd = op & 0x7
        
        addr = (self.regs[rb] + offset) & 0xFFFFFFFF
        
        if load:
            self.regs[rd] = self.mmu.read16(addr)
            return 3
        else:
            self.mmu.write16(addr, self.regs[rd] & 0xFFFF)
            return 2
    
    def _thumb_sp_load_store(self, op):
        """Thumb SP-relative Load/Store"""
        load = bool(op & 0x0800)
        rd = (op >> 8) & 0x7
        offset = (op & 0xFF) << 2
        
        addr = (self.regs[13] + offset) & 0xFFFFFFFF
        
        if load:
            self.regs[rd] = self.mmu.read32(addr)
            return 3
        else:
            self.mmu.write32(addr, self.regs[rd])
            return 2
    
    def _thumb_load_addr(self, op):
        """Thumb Load Address"""
        sp = bool(op & 0x0800)
        rd = (op >> 8) & 0x7
        offset = (op & 0xFF) << 2
        
        if sp:
            self.regs[rd] = (self.regs[13] + offset) & 0xFFFFFFFF
        else:
            self.regs[rd] = ((self.regs[15] + 2) & ~2) + offset
        
        return 1
    
    def _thumb_sp_add(self, op):
        """Thumb Add Offset to SP / Push-Pop / Misc"""
        if (op & 0x0F00) == 0x0000:
            # ADD SP, #imm
            offset = (op & 0x7F) << 2
            if op & 0x80:
                self.regs[13] = (self.regs[13] - offset) & 0xFFFFFFFF
            else:
                self.regs[13] = (self.regs[13] + offset) & 0xFFFFFFFF
            return 1
        else:
            return self._thumb_push_pop(op)
    
    def _thumb_push_pop(self, op):
        """Thumb Push/Pop Registers"""
        load = bool(op & 0x0800)
        pclr = bool(op & 0x0100)
        rlist = op & 0xFF
        
        addr = self.regs[13]
        cycles = 1
        
        if load:  # POP
            for i in range(8):
                if rlist & (1 << i):
                    self.regs[i] = self.mmu.read32(addr)
                    addr = (addr + 4) & 0xFFFFFFFF
                    cycles += 1
            
            if pclr:
                val = self.mmu.read32(addr)
                self.thumb = bool(val & 1)
                self.cpsr = (self.cpsr & ~0x20) | (0x20 if self.thumb else 0)
                self.regs[15] = val & ~1
                addr = (addr + 4) & 0xFFFFFFFF
                self._flush_pipeline()
                cycles += 1
            
            self.regs[13] = addr
        else:  # PUSH
            if pclr:
                addr = (addr - 4) & 0xFFFFFFFF
                self.mmu.write32(addr, self.regs[14])
                cycles += 1
            
            for i in range(7, -1, -1):
                if rlist & (1 << i):
                    addr = (addr - 4) & 0xFFFFFFFF
                    self.mmu.write32(addr, self.regs[i])
                    cycles += 1
            
            self.regs[13] = addr
        
        return cycles
    
    def _thumb_stmia_ldmia(self, op):
        """Thumb Multiple Load/Store"""
        load = bool(op & 0x0800)
        rb = (op >> 8) & 0x7
        rlist = op & 0xFF
        
        addr = self.regs[rb]
        cycles = 1
        
        if load:
            for i in range(8):
                if rlist & (1 << i):
                    self.regs[i] = self.mmu.read32(addr)
                    addr = (addr + 4) & 0xFFFFFFFF
                    cycles += 1
        else:
            for i in range(8):
                if rlist & (1 << i):
                    self.mmu.write32(addr, self.regs[i])
                    addr = (addr + 4) & 0xFFFFFFFF
                    cycles += 1
        
        self.regs[rb] = addr
        return cycles
    
    def _thumb_cond_branch(self, op):
        """Thumb Conditional Branch"""
        cond = (op >> 8) & 0xF
        offset = op & 0xFF
        if offset & 0x80:
            offset |= 0xFFFFFF00
        offset = (offset << 1) & 0xFFFFFFFF
        if offset & 0x80000000:
            offset = offset - 0x100000000
        
        if self.check_condition(cond):
            self.regs[15] = (self.regs[15] + offset + 2) & 0xFFFFFFFF
            self._flush_pipeline()
            return 3
        return 1
    
    def _thumb_swi(self, op):
        """Thumb Software Interrupt"""
        # Save CPSR
        mode_idx = self._get_mode_index(MODE_SVC)
        self.spsr[mode_idx] = self.cpsr
        
        # Switch to SVC mode
        self._switch_mode(MODE_SVC)
        
        # Save return address
        self.regs[14] = (self.regs[15] - 2) & 0xFFFFFFFF
        
        # Jump to SWI vector
        self.regs[15] = 0x00000008
        
        # Disable IRQs, switch to ARM
        self.cpsr |= 0x80
        self.cpsr &= ~0x20
        self.irq_disable = True
        self.thumb = False
        
        self._flush_pipeline()
        return 3
    
    def _thumb_uncond_branch(self, op):
        """Thumb Unconditional Branch"""
        offset = op & 0x7FF
        if offset & 0x400:
            offset |= 0xFFFFF800
        offset = (offset << 1) & 0xFFFFFFFF
        if offset & 0x80000000:
            offset = offset - 0x100000000
        
        self.regs[15] = (self.regs[15] + offset + 2) & 0xFFFFFFFF
        self._flush_pipeline()
        return 3
    
    def _thumb_long_branch(self, op):
        """Thumb Long Branch with Link"""
        h = (op >> 11) & 0x3
        offset = op & 0x7FF
        
        if h == 0b10:
            # First instruction - set LR
            if offset & 0x400:
                offset |= 0xFFFFF800
            self.regs[14] = (self.regs[15] + 2 + (offset << 12)) & 0xFFFFFFFF
            return 1
        elif h == 0b11:
            # Second instruction - branch
            tmp = self.regs[15]
            self.regs[15] = (self.regs[14] + (offset << 1)) & 0xFFFFFFFE
            self.regs[14] = (tmp - 2) | 1
            self._flush_pipeline()
            return 3
        elif h == 0b01:
            # BLX (branch to ARM)
            tmp = self.regs[15]
            self.regs[15] = (self.regs[14] + (offset << 1)) & 0xFFFFFFFC
            self.regs[14] = (tmp - 2) | 1
            self.thumb = False
            self.cpsr &= ~0x20
            self._flush_pipeline()
            return 3
        
        return 1

# =============================================================================
# PPU (Picture Processing Unit)
# =============================================================================

class PPU:
    """GBA Picture Processing Unit - Handles all graphics rendering"""
    
    def __init__(self, mmu):
        self.mmu = mmu
        mmu.ppu = self
        
        # State
        self.vcount = 0
        self.hblank = False
        self.cycles = 0
        
        # Framebuffer (RGB888)
        self.framebuffer = bytearray(SCREEN_W * SCREEN_H * 3)
        
        # Line buffer for compositing
        self.line_buffer = [(0, 0, 4)] * SCREEN_W  # (color, priority, layer)
        
        # Affine background internal registers
        self.bg2_ref_x = 0
        self.bg2_ref_y = 0
        self.bg3_ref_x = 0
        self.bg3_ref_y = 0
        
        # OBJ line buffer
        self.obj_buffer = [(0, 4, False)] * SCREEN_W  # (color, priority, semi_transparent)
    
    def step(self, cycles):
        """Step PPU by given cycles"""
        self.cycles += cycles
        
        while self.cycles >= 4:
            self.cycles -= 4
            self._tick()
    
    def _tick(self):
        """Internal PPU tick"""
        # Each scanline is 1232 cycles (308 dots)
        # HBlank starts at cycle 1006 (dot 252)
        # There are 228 scanlines total (160 visible + 68 vblank)
        
        dot = (self.cycles // 4) % 308
        
        # Check for HBlank transition
        if dot == 252 and not self.hblank:
            self.hblank = True
            
            # Render scanline at start of HBlank (if visible)
            if self.vcount < SCREEN_H:
                self._render_scanline(self.vcount)
            
            # HBlank interrupt
            if self.mmu.io[REG_DISPSTAT] & 0x10:
                self.mmu.request_irq(IRQ_HBLANK)
            
            # HBlank DMA
            if self.vcount < SCREEN_H:
                self.mmu.trigger_dma(2)  # HBlank timing
        
        elif dot == 0 and self.hblank:
            self.hblank = False
            self.vcount = (self.vcount + 1) % 228
            
            # VBlank start
            if self.vcount == SCREEN_H:
                # VBlank interrupt
                if self.mmu.io[REG_DISPSTAT] & 0x08:
                    self.mmu.request_irq(IRQ_VBLANK)
                
                # VBlank DMA
                self.mmu.trigger_dma(1)
                
                # Reset affine reference points
                self._reload_affine_refs()
            
            # V-Count match
            lyc = self.mmu.io[REG_DISPSTAT + 1]
            if self.vcount == lyc:
                if self.mmu.io[REG_DISPSTAT] & 0x20:
                    self.mmu.request_irq(IRQ_VCOUNT)
    
    def _reload_affine_refs(self):
        """Reload affine background reference points"""
        # BG2 reference point
        x = (self.mmu.io[REG_BG2X] | (self.mmu.io[REG_BG2X + 1] << 8) |
             (self.mmu.io[REG_BG2X + 2] << 16) | (self.mmu.io[REG_BG2X + 3] << 24))
        y = (self.mmu.io[REG_BG2Y] | (self.mmu.io[REG_BG2Y + 1] << 8) |
             (self.mmu.io[REG_BG2Y + 2] << 16) | (self.mmu.io[REG_BG2Y + 3] << 24))
        self.bg2_ref_x = sign_extend(x, 28)
        self.bg2_ref_y = sign_extend(y, 28)
        
        # BG3 reference point
        x = (self.mmu.io[REG_BG3X] | (self.mmu.io[REG_BG3X + 1] << 8) |
             (self.mmu.io[REG_BG3X + 2] << 16) | (self.mmu.io[REG_BG3X + 3] << 24))
        y = (self.mmu.io[REG_BG3Y] | (self.mmu.io[REG_BG3Y + 1] << 8) |
             (self.mmu.io[REG_BG3Y + 2] << 16) | (self.mmu.io[REG_BG3Y + 3] << 24))
        self.bg3_ref_x = sign_extend(x, 28)
        self.bg3_ref_y = sign_extend(y, 28)
    
    def _render_scanline(self, line):
        """Render a single scanline"""
        dispcnt = self.mmu.io[REG_DISPCNT] | (self.mmu.io[REG_DISPCNT + 1] << 8)
        mode = dispcnt & 0x7
        
        # Clear line buffer
        backdrop = self._get_palette_color(0)
        for x in range(SCREEN_W):
            self.line_buffer[x] = (backdrop, 4, 5)  # backdrop is layer 5
            self.obj_buffer[x] = (0, 4, False)
        
        # Forced blank
        if dispcnt & 0x80:
            self._write_line(line, (255, 255, 255))
            return
        
        # Render based on mode
        if mode == 0:
            self._render_mode0(line, dispcnt)
        elif mode == 1:
            self._render_mode1(line, dispcnt)
        elif mode == 2:
            self._render_mode2(line, dispcnt)
        elif mode == 3:
            self._render_mode3(line, dispcnt)
        elif mode == 4:
            self._render_mode4(line, dispcnt)
        elif mode == 5:
            self._render_mode5(line, dispcnt)
        
        # Render OBJs
        if dispcnt & 0x1000:
            self._render_objs(line)
        
        # Composite and write to framebuffer
        self._composite_line(line)
    
    def _get_palette_color(self, index, obj=False):
        """Get color from palette RAM"""
        base = 0x200 if obj else 0
        addr = base + index * 2
        if addr >= PALETTE_SIZE:
            return (0, 0, 0)
        color = self.mmu.palette[addr] | (self.mmu.palette[addr + 1] << 8)
        return rgb555_to_rgb888(color)
    
    def _write_line(self, line, color):
        """Write solid color to framebuffer line"""
        base = line * SCREEN_W * 3
        for x in range(SCREEN_W):
            idx = base + x * 3
            self.framebuffer[idx] = color[0]
            self.framebuffer[idx + 1] = color[1]
            self.framebuffer[idx + 2] = color[2]
    
    def _composite_line(self, line):
        """Composite all layers and write to framebuffer"""
        base = line * SCREEN_W * 3
        
        for x in range(SCREEN_W):
            # Get background pixel
            bg_color, bg_prio, bg_layer = self.line_buffer[x]
            
            # Get OBJ pixel
            obj_color, obj_prio, obj_semi = self.obj_buffer[x]
            
            # Priority comparison
            if obj_color != (0, 0, 0) and obj_prio <= bg_prio:
                color = obj_color
            else:
                color = bg_color
            
            idx = base + x * 3
            self.framebuffer[idx] = color[0]
            self.framebuffer[idx + 1] = color[1]
            self.framebuffer[idx + 2] = color[2]
    
    def _render_mode0(self, line, dispcnt):
        """Render Mode 0 (4 text BGs)"""
        # Render BGs in priority order (3 to 0)
        for priority in range(3, -1, -1):
            for bg in range(3, -1, -1):
                if dispcnt & (0x100 << bg):
                    bg_cnt = self.mmu.io[REG_BG0CNT + bg * 2] | (self.mmu.io[REG_BG0CNT + bg * 2 + 1] << 8)
                    if (bg_cnt & 3) == priority:
                        self._render_text_bg(line, bg, bg_cnt)
    
    def _render_mode1(self, line, dispcnt):
        """Render Mode 1 (2 text + 1 affine BG)"""
        for priority in range(3, -1, -1):
            # BG0 and BG1 are text
            for bg in range(2):
                if dispcnt & (0x100 << bg):
                    bg_cnt = self.mmu.io[REG_BG0CNT + bg * 2] | (self.mmu.io[REG_BG0CNT + bg * 2 + 1] << 8)
                    if (bg_cnt & 3) == priority:
                        self._render_text_bg(line, bg, bg_cnt)
            
            # BG2 is affine
            if dispcnt & 0x400:
                bg_cnt = self.mmu.io[REG_BG2CNT] | (self.mmu.io[REG_BG2CNT + 1] << 8)
                if (bg_cnt & 3) == priority:
                    self._render_affine_bg(line, 2, bg_cnt)
    
    def _render_mode2(self, line, dispcnt):
        """Render Mode 2 (2 affine BGs)"""
        for priority in range(3, -1, -1):
            for bg in [2, 3]:
                if dispcnt & (0x100 << bg):
                    bg_cnt = self.mmu.io[REG_BG0CNT + bg * 2] | (self.mmu.io[REG_BG0CNT + bg * 2 + 1] << 8)
                    if (bg_cnt & 3) == priority:
                        self._render_affine_bg(line, bg, bg_cnt)
    
    def _render_mode3(self, line, dispcnt):
        """Render Mode 3 (240x160 15-bit bitmap)"""
        if not (dispcnt & 0x400):
            return
        
        for x in range(SCREEN_W):
            addr = (line * SCREEN_W + x) * 2
            if addr < VRAM_SIZE:
                color = self.mmu.vram[addr] | (self.mmu.vram[addr + 1] << 8)
                rgb = rgb555_to_rgb888(color)
                self.line_buffer[x] = (rgb, 0, 2)
    
    def _render_mode4(self, line, dispcnt):
        """Render Mode 4 (240x160 8-bit paletted)"""
        if not (dispcnt & 0x400):
            return
        
        # Frame select
        base = 0xA000 if (dispcnt & 0x10) else 0
        
        for x in range(SCREEN_W):
            addr = base + line * SCREEN_W + x
            if addr < VRAM_SIZE:
                idx = self.mmu.vram[addr]
                if idx != 0:
                    rgb = self._get_palette_color(idx)
                    self.line_buffer[x] = (rgb, 0, 2)
    
    def _render_mode5(self, line, dispcnt):
        """Render Mode 5 (160x128 15-bit bitmap)"""
        if not (dispcnt & 0x400):
            return
        
        if line >= 128:
            return
        
        # Frame select
        base = 0xA000 if (dispcnt & 0x10) else 0
        
        # Center on screen
        x_off = (SCREEN_W - 160) // 2
        
        for x in range(160):
            addr = base + (line * 160 + x) * 2
            if addr + 1 < VRAM_SIZE:
                color = self.mmu.vram[addr] | (self.mmu.vram[addr + 1] << 8)
                rgb = rgb555_to_rgb888(color)
                self.line_buffer[x + x_off] = (rgb, 0, 2)
    
    def _render_text_bg(self, line, bg, bg_cnt):
        """Render a text mode background layer"""
        priority = bg_cnt & 3
        char_base = ((bg_cnt >> 2) & 3) * 0x4000
        mosaic = bool(bg_cnt & 0x40)
        palette_256 = bool(bg_cnt & 0x80)
        map_base = ((bg_cnt >> 8) & 0x1F) * 0x800
        wrap = bool(bg_cnt & 0x2000)
        size = (bg_cnt >> 14) & 3
        
        # Size in tiles
        sizes = [(32, 32), (64, 32), (32, 64), (64, 64)]
        width, height = sizes[size]
        
        # Scroll
        h_off = self.mmu.io[REG_BG0HOFS + bg * 4] | ((self.mmu.io[REG_BG0HOFS + bg * 4 + 1] & 1) << 8)
        v_off = self.mmu.io[REG_BG0VOFS + bg * 4] | ((self.mmu.io[REG_BG0VOFS + bg * 4 + 1] & 1) << 8)
        
        y = (line + v_off) & 0x1FF
        
        for screen_x in range(SCREEN_W):
            x = (screen_x + h_off) & 0x1FF
            
            # Get tile coordinates
            tile_x = x >> 3
            tile_y = y >> 3
            
            # Handle screen overflow
            map_offset = 0
            if tile_x >= 32:
                if width == 64:
                    map_offset += 0x800
                tile_x &= 31
            if tile_y >= 32:
                if height == 64:
                    map_offset += 0x800 if width == 32 else 0x1000
                tile_y &= 31
            
            # Get tile entry
            tile_addr = map_base + map_offset + (tile_y * 32 + tile_x) * 2
            tile_entry = self.mmu.vram[tile_addr] | (self.mmu.vram[tile_addr + 1] << 8)
            
            tile_num = tile_entry & 0x3FF
            h_flip = bool(tile_entry & 0x400)
            v_flip = bool(tile_entry & 0x800)
            palette_num = (tile_entry >> 12) & 0xF
            
            # Get pixel within tile
            px = x & 7
            py = y & 7
            
            if h_flip:
                px = 7 - px
            if v_flip:
                py = 7 - py
            
            # Get pixel color
            if palette_256:
                pixel_addr = char_base + tile_num * 64 + py * 8 + px
                color_idx = self.mmu.vram[pixel_addr] if pixel_addr < VRAM_SIZE else 0
            else:
                pixel_addr = char_base + tile_num * 32 + py * 4 + (px >> 1)
                if pixel_addr < VRAM_SIZE:
                    byte = self.mmu.vram[pixel_addr]
                    color_idx = (byte >> 4) if (px & 1) else (byte & 0xF)
                    if color_idx:
                        color_idx += palette_num * 16
                else:
                    color_idx = 0
            
            if color_idx != 0:
                rgb = self._get_palette_color(color_idx)
                cur_prio = self.line_buffer[screen_x][1]
                if priority <= cur_prio:
                    self.line_buffer[screen_x] = (rgb, priority, bg)
    
    def _render_affine_bg(self, line, bg, bg_cnt):
        """Render an affine (rotation/scaling) background"""
        priority = bg_cnt & 3
        char_base = ((bg_cnt >> 2) & 3) * 0x4000
        mosaic = bool(bg_cnt & 0x40)
        map_base = ((bg_cnt >> 8) & 0x1F) * 0x800
        wrap = bool(bg_cnt & 0x2000)
        size = (bg_cnt >> 14) & 3
        
        # Size in pixels
        sizes = [128, 256, 512, 1024]
        map_size = sizes[size]
        tile_count = map_size // 8
        
        # Get affine parameters
        if bg == 2:
            pa = sign_extend(self.mmu.io[REG_BG2PA] | (self.mmu.io[REG_BG2PA + 1] << 8), 16)
            pc = sign_extend(self.mmu.io[REG_BG2PC] | (self.mmu.io[REG_BG2PC + 1] << 8), 16)
            ref_x = self.bg2_ref_x
            ref_y = self.bg2_ref_y
        else:
            pa = sign_extend(self.mmu.io[REG_BG3PA] | (self.mmu.io[REG_BG3PA + 1] << 8), 16)
            pc = sign_extend(self.mmu.io[REG_BG3PC] | (self.mmu.io[REG_BG3PC + 1] << 8), 16)
            ref_x = self.bg3_ref_x
            ref_y = self.bg3_ref_y
        
        # Calculate starting position
        x_pos = ref_x
        y_pos = ref_y
        
        for screen_x in range(SCREEN_W):
            # Convert fixed point to integer
            tex_x = x_pos >> 8
            tex_y = y_pos >> 8
            
            # Wrapping
            if wrap:
                tex_x &= (map_size - 1)
                tex_y &= (map_size - 1)
            elif tex_x < 0 or tex_x >= map_size or tex_y < 0 or tex_y >= map_size:
                x_pos += pa
                y_pos += pc
                continue
            
            # Get tile
            tile_x = tex_x >> 3
            tile_y = tex_y >> 3
            
            tile_addr = map_base + tile_y * tile_count + tile_x
            tile_num = self.mmu.vram[tile_addr] if tile_addr < VRAM_SIZE else 0
            
            # Get pixel
            px = tex_x & 7
            py = tex_y & 7
            
            pixel_addr = char_base + tile_num * 64 + py * 8 + px
            color_idx = self.mmu.vram[pixel_addr] if pixel_addr < VRAM_SIZE else 0
            
            if color_idx != 0:
                rgb = self._get_palette_color(color_idx)
                cur_prio = self.line_buffer[screen_x][1]
                if priority <= cur_prio:
                    self.line_buffer[screen_x] = (rgb, priority, bg)
            
            x_pos += pa
            y_pos += pc
        
        # Update reference points for next line
        if bg == 2:
            pb = sign_extend(self.mmu.io[REG_BG2PB] | (self.mmu.io[REG_BG2PB + 1] << 8), 16)
            pd = sign_extend(self.mmu.io[REG_BG2PD] | (self.mmu.io[REG_BG2PD + 1] << 8), 16)
            self.bg2_ref_x += pb
            self.bg2_ref_y += pd
        else:
            pb = sign_extend(self.mmu.io[REG_BG3PB] | (self.mmu.io[REG_BG3PB + 1] << 8), 16)
            pd = sign_extend(self.mmu.io[REG_BG3PD] | (self.mmu.io[REG_BG3PD + 1] << 8), 16)
            self.bg3_ref_x += pb
            self.bg3_ref_y += pd
    
    def _render_objs(self, line):
        """Render all sprites for current line"""
        dispcnt = self.mmu.io[REG_DISPCNT] | (self.mmu.io[REG_DISPCNT + 1] << 8)
        obj_mapping = bool(dispcnt & 0x40)  # 1D or 2D mapping
        
        # Process OAM entries (128 sprites, back to front for priority)
        for i in range(127, -1, -1):
            base = i * 8
            
            # Attribute 0
            attr0 = self.mmu.oam[base] | (self.mmu.oam[base + 1] << 8)
            y = attr0 & 0xFF
            affine = bool(attr0 & 0x100)
            double_size = bool(attr0 & 0x200)
            obj_disable = bool(attr0 & 0x200) and not affine
            mode = (attr0 >> 10) & 3
            mosaic = bool(attr0 & 0x1000)
            palette_256 = bool(attr0 & 0x2000)
            shape = (attr0 >> 14) & 3
            
            if obj_disable:
                continue
            
            # Attribute 1
            attr1 = self.mmu.oam[base + 2] | (self.mmu.oam[base + 3] << 8)
            x = attr1 & 0x1FF
            if x >= 240:
                x -= 512
            affine_idx = (attr1 >> 9) & 0x1F
            h_flip = bool(attr1 & 0x1000) and not affine
            v_flip = bool(attr1 & 0x2000) and not affine
            size = (attr1 >> 14) & 3
            
            # Attribute 2
            attr2 = self.mmu.oam[base + 4] | (self.mmu.oam[base + 5] << 8)
            tile_num = attr2 & 0x3FF
            priority = (attr2 >> 10) & 3
            palette_num = (attr2 >> 12) & 0xF
            
            # Get sprite dimensions
            obj_sizes = [
                [(8, 8), (16, 16), (32, 32), (64, 64)],    # Square
                [(16, 8), (32, 8), (32, 16), (64, 32)],    # Horizontal
                [(8, 16), (8, 32), (16, 32), (32, 64)],    # Vertical
                [(8, 8), (8, 8), (8, 8), (8, 8)]           # Invalid
            ]
            width, height = obj_sizes[shape][size]
            
            # Check if sprite is on this line
            render_height = height * 2 if (affine and double_size) else height
            
            if y > line or y + render_height <= line:
                if y > 160:  # Handle wrap
                    y -= 256
                    if y > line or y + render_height <= line:
                        continue
                else:
                    continue
            
            # Get sprite line
            sprite_y = line - y
            
            if affine:
                self._render_affine_obj(x, sprite_y, width, height, tile_num, palette_256,
                                       palette_num, priority, affine_idx, double_size, obj_mapping, mode)
            else:
                if v_flip:
                    sprite_y = height - 1 - sprite_y
                
                self._render_normal_obj(x, sprite_y, width, height, tile_num, palette_256,
                                        palette_num, priority, h_flip, obj_mapping, mode)
    
    def _render_normal_obj(self, x, sprite_y, width, height, tile_num, palette_256, 
                          palette_num, priority, h_flip, obj_mapping, mode):
        """Render a normal (non-affine) sprite line"""
        for px in range(width):
            screen_x = x + (width - 1 - px if h_flip else px)
            
            if screen_x < 0 or screen_x >= SCREEN_W:
                continue
            
            # Skip if higher priority obj already here
            if self.obj_buffer[screen_x][1] < priority:
                continue
            
            # Calculate tile coordinates
            tile_x = px >> 3
            tile_y = sprite_y >> 3
            
            if obj_mapping:  # 1D mapping
                tile_idx = tile_num + tile_y * (width >> 3) + tile_x
            else:  # 2D mapping
                tile_idx = tile_num + tile_y * 32 + tile_x
            
            if palette_256:
                tile_idx &= ~1  # 256-color tiles take 2 slots
            
            # Get pixel within tile
            sub_x = px & 7
            sub_y = sprite_y & 7
            
            # Get pixel color
            char_base = 0x10000  # OBJ character base
            
            if palette_256:
                pixel_addr = char_base + tile_idx * 32 + sub_y * 8 + sub_x
                color_idx = self.mmu.vram[pixel_addr] if pixel_addr < VRAM_SIZE else 0
            else:
                pixel_addr = char_base + tile_idx * 32 + sub_y * 4 + (sub_x >> 1)
                if pixel_addr < VRAM_SIZE:
                    byte = self.mmu.vram[pixel_addr]
                    color_idx = (byte >> 4) if (sub_x & 1) else (byte & 0xF)
                    if color_idx:
                        color_idx += palette_num * 16
                else:
                    color_idx = 0
            
            if color_idx != 0:
                rgb = self._get_palette_color(color_idx, obj=True)
                self.obj_buffer[screen_x] = (rgb, priority, mode == 1)
    
    def _render_affine_obj(self, x, sprite_y, width, height, tile_num, palette_256,
                          palette_num, priority, affine_idx, double_size, obj_mapping, mode):
        """Render an affine-transformed sprite line"""
        # Get affine parameters from OAM
        param_base = affine_idx * 32 + 6
        pa = sign_extend(self.mmu.oam[param_base] | (self.mmu.oam[param_base + 1] << 8), 16)
        pb = sign_extend(self.mmu.oam[param_base + 8] | (self.mmu.oam[param_base + 9] << 8), 16)
        pc = sign_extend(self.mmu.oam[param_base + 16] | (self.mmu.oam[param_base + 17] << 8), 16)
        pd = sign_extend(self.mmu.oam[param_base + 24] | (self.mmu.oam[param_base + 25] << 8), 16)
        
        # Calculate render dimensions
        render_width = width * 2 if double_size else width
        render_height = height * 2 if double_size else height
        
        # Center of sprite
        cx = width / 2
        cy = height / 2
        
        for px in range(render_width):
            screen_x = x + px
            
            if screen_x < 0 or screen_x >= SCREEN_W:
                continue
            
            if self.obj_buffer[screen_x][1] < priority:
                continue
            
            # Transform pixel coordinates
            rx = px - render_width / 2
            ry = sprite_y - render_height / 2
            
            tex_x = ((pa * rx + pb * ry) >> 8) + cx
            tex_y = ((pc * rx + pd * ry) >> 8) + cy
            
            tex_x = int(tex_x)
            tex_y = int(tex_y)
            
            if tex_x < 0 or tex_x >= width or tex_y < 0 or tex_y >= height:
                continue
            
            # Get tile
            tile_x = tex_x >> 3
            tile_y = tex_y >> 3
            
            if obj_mapping:
                tile_idx = tile_num + tile_y * (width >> 3) + tile_x
            else:
                tile_idx = tile_num + tile_y * 32 + tile_x
            
            if palette_256:
                tile_idx &= ~1
            
            sub_x = tex_x & 7
            sub_y = tex_y & 7
            
            char_base = 0x10000
            
            if palette_256:
                pixel_addr = char_base + tile_idx * 32 + sub_y * 8 + sub_x
                color_idx = self.mmu.vram[pixel_addr] if pixel_addr < VRAM_SIZE else 0
            else:
                pixel_addr = char_base + tile_idx * 32 + sub_y * 4 + (sub_x >> 1)
                if pixel_addr < VRAM_SIZE:
                    byte = self.mmu.vram[pixel_addr]
                    color_idx = (byte >> 4) if (sub_x & 1) else (byte & 0xF)
                    if color_idx:
                        color_idx += palette_num * 16
                else:
                    color_idx = 0
            
            if color_idx != 0:
                rgb = self._get_palette_color(color_idx, obj=True)
                self.obj_buffer[screen_x] = (rgb, priority, mode == 1)

# =============================================================================
# APU (Audio Processing Unit) - Basic implementation
# =============================================================================

class APU:
    """GBA Audio Processing Unit - Basic sound support"""
    
    def __init__(self, mmu):
        self.mmu = mmu
        mmu.apu = self
        
        # Sound channels
        self.channel_enable = [False] * 4
        
        # FIFO buffers
        self.fifo_a = []
        self.fifo_b = []
        
        # Master enable
        self.enabled = False
    
    def timer_overflow(self, timer):
        """Called when timer 0 or 1 overflows (for FIFO)"""
        pass  # Would handle FIFO audio here

# =============================================================================
# EMULATOR UI
# =============================================================================

class EmulatorUI:
    """GBA Emulator GUI using Tkinter"""
    
    def __init__(self, root):
        self.root = root
        self.root.title("GBA Emulator - Team Flames / Cat OS")
        self.root.resizable(False, False)
        
        # Create canvas
        self.canvas = tk.Canvas(root, width=SCREEN_W * SCALE, height=SCREEN_H * SCALE, bg="black")
        self.canvas.pack()
        
        # PhotoImage for display
        self.img = tk.PhotoImage(width=SCREEN_W, height=SCREEN_H)
        self.canvas_img = self.canvas.create_image(0, 0, image=self.img, anchor="nw")
        self.canvas.scale("all", 0, 0, SCALE, SCALE)
        
        # Initialize emulator components
        self.mmu = MMU()
        self.cpu = ARM7TDMI(self.mmu)
        self.ppu = PPU(self.mmu)
        self.apu = APU(self.mmu)
        
        # State
        self.running = False
        self.paused = False
        self.rom_loaded = False
        
        # Setup
        self._setup_menu()
        self._setup_controls()
        self._setup_status_bar()
        
        # Show test pattern
        self._show_test_pattern()
        
        # Bind close event
        self.root.protocol("WM_DELETE_WINDOW", self._on_close)
    
    def _setup_menu(self):
        """Setup menu bar"""
        menubar = tk.Menu(self.root)
        
        # File menu
        file_menu = tk.Menu(menubar, tearoff=0)
        file_menu.add_command(label="Open ROM...", command=self._load_rom, accelerator="Ctrl+O")
        file_menu.add_command(label="Load BIOS...", command=self._load_bios)
        file_menu.add_separator()
        file_menu.add_command(label="Reset", command=self._reset, accelerator="Ctrl+R")
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self._on_close)
        menubar.add_cascade(label="File", menu=file_menu)
        
        # Emulation menu
        emu_menu = tk.Menu(menubar, tearoff=0)
        emu_menu.add_command(label="Pause/Resume", command=self._toggle_pause, accelerator="P")
        emu_menu.add_command(label="Frame Advance", command=self._frame_advance, accelerator="N")
        menubar.add_cascade(label="Emulation", menu=emu_menu)
        
        # Help menu
        help_menu = tk.Menu(menubar, tearoff=0)
        help_menu.add_command(label="Controls", command=self._show_controls)
        help_menu.add_command(label="About", command=self._show_about)
        menubar.add_cascade(label="Help", menu=help_menu)
        
        self.root.config(menu=menubar)
        
        # Keyboard shortcuts
        self.root.bind("<Control-o>", lambda e: self._load_rom())
        self.root.bind("<Control-r>", lambda e: self._reset())
        self.root.bind("<p>", lambda e: self._toggle_pause())
        self.root.bind("<n>", lambda e: self._frame_advance())
    
    def _setup_controls(self):
        """Setup keyboard controls"""
        self.key_map = {
            'z': KEY_A,
            'x': KEY_B,
            'Return': KEY_START,
            'BackSpace': KEY_SELECT,
            'Up': KEY_UP,
            'Down': KEY_DOWN,
            'Left': KEY_LEFT,
            'Right': KEY_RIGHT,
            'a': KEY_L,
            's': KEY_R
        }
        
        self.root.bind("<KeyPress>", self._key_down)
        self.root.bind("<KeyRelease>", self._key_up)
    
    def _setup_status_bar(self):
        """Setup status bar"""
        self.status_var = tk.StringVar(value="Ready - Load a ROM to start")
        status_bar = ttk.Label(self.root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W)
        status_bar.pack(fill=tk.X, side=tk.BOTTOM)
    
    def _key_down(self, event):
        """Handle key press"""
        if event.keysym in self.key_map:
            bit = self.key_map[event.keysym]
            self.mmu.key_state &= ~(1 << bit)
    
    def _key_up(self, event):
        """Handle key release"""
        if event.keysym in self.key_map:
            bit = self.key_map[event.keysym]
            self.mmu.key_state |= (1 << bit)
    
    def _show_test_pattern(self):
        """Display test pattern"""
        # Setup rainbow palette
        for i in range(256):
            h = (i / 256) * 6
            sector = int(h)
            f = h - sector
            
            if sector == 0:
                r, g, b = 31, int(31 * f), 0
            elif sector == 1:
                r, g, b = int(31 * (1 - f)), 31, 0
            elif sector == 2:
                r, g, b = 0, 31, int(31 * f)
            elif sector == 3:
                r, g, b = 0, int(31 * (1 - f)), 31
            elif sector == 4:
                r, g, b = int(31 * f), 0, 31
            else:
                r, g, b = 31, 0, int(31 * (1 - f))
            
            color = r | (g << 5) | (b << 10)
            self.mmu.palette[i * 2] = color & 0xFF
            self.mmu.palette[i * 2 + 1] = (color >> 8) & 0xFF
        
        # Fill VRAM with pattern
        for y in range(SCREEN_H):
            for x in range(SCREEN_W):
                self.mmu.vram[y * SCREEN_W + x] = (x + y * 2) % 256
        
        # Render
        for line in range(SCREEN_H):
            self.ppu._render_scanline(line)
        
        self._update_display()
    
    def _update_display(self):
        """Update canvas with framebuffer"""
        # Build PPM data
        header = f"P6\n{SCREEN_W} {SCREEN_H}\n255\n"
        data = header.encode() + bytes(self.ppu.framebuffer)
        
        try:
            self.img.configure(data=data)
        except Exception:
            pass
    
    def _load_rom(self):
        """Load ROM file"""
        path = filedialog.askopenfilename(
            title="Open GBA ROM",
            filetypes=[("GBA ROM", "*.gba"), ("All Files", "*.*")]
        )
        
        if path:
            try:
                with open(path, "rb") as f:
                    self.mmu.load_rom(f.read())
                
                self.rom_loaded = True
                self._reset()
                
                if not self.running:
                    self.running = True
                    self.paused = False
                    threading.Thread(target=self._main_loop, daemon=True).start()
                
                self.status_var.set(f"Running: {os.path.basename(path)}")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to load ROM: {e}")
    
    def _load_bios(self):
        """Load BIOS file"""
        path = filedialog.askopenfilename(
            title="Open GBA BIOS",
            filetypes=[("GBA BIOS", "*.bin"), ("All Files", "*.*")]
        )
        
        if path:
            try:
                with open(path, "rb") as f:
                    self.mmu.load_bios(f.read())
                messagebox.showinfo("Success", "BIOS loaded successfully")
            except Exception as e:
                messagebox.showerror("Error", f"Failed to load BIOS: {e}")
    
    def _reset(self):
        """Reset emulator"""
        self.cpu.reset()
        self.ppu.vcount = 0
        self.ppu._reload_affine_refs()
        self.mmu.halt = False
        self.mmu.stop = False
        self.mmu._init_io()
    
    def _toggle_pause(self):
        """Toggle pause state"""
        self.paused = not self.paused
        self.status_var.set("Paused" if self.paused else "Running")
    
    def _frame_advance(self):
        """Advance single frame"""
        if self.rom_loaded:
            self.paused = True
            self._run_frame()
            self._update_display()
    
    def _main_loop(self):
        """Main emulation loop"""
        target_time = 1.0 / FPS
        
        while self.running:
            if self.paused:
                time.sleep(0.016)
                continue
            
            start = time.time()
            
            self._run_frame()
            
            # Schedule UI update
            self.root.after(0, self._update_display)
            
            # Frame timing
            elapsed = time.time() - start
            sleep_time = target_time - elapsed
            if sleep_time > 0:
                time.sleep(sleep_time)
    
    def _run_frame(self):
        """Run one frame of emulation"""
        frame_cycles = 0
        
        while frame_cycles < CYCLES_PER_FRAME:
            cycles = self.cpu.step()
            frame_cycles += cycles
            
            # Update subsystems
            self.mmu.update_timers(cycles)
            self.ppu.step(cycles)
    
    def _show_controls(self):
        """Show controls dialog"""
        controls = """
GBA Emulator Controls:

Movement:
  Arrow Keys - D-Pad

Buttons:
  Z - A Button
  X - B Button
  Enter - Start
  Backspace - Select
  A - L Trigger
  S - R Trigger

Emulation:
  P - Pause/Resume
  N - Frame Advance
  Ctrl+R - Reset
  Ctrl+O - Open ROM
"""
        messagebox.showinfo("Controls", controls)
    
    def _show_about(self):
        """Show about dialog"""
        about = """
GBA Emulator - Pure Python Edition
Version 1.0

Team Flames / Samsoft / Cat OS

Features:
• ARM7TDMI CPU (ARM + Thumb)
• All Video Modes (0-5)
• DMA, Timers, Interrupts
• Sprite/Background rendering
• 60 FPS target

For educational purposes only.
        """
        messagebox.showinfo("About", about)
    
    def _on_close(self):
        """Handle window close"""
        self.running = False
        self.root.destroy()

# =============================================================================
# ENTRY POINT
# =============================================================================

if __name__ == "__main__":
    print("╔════════════════════════════════════════════════════════════════════════════════════════╗")
    print("║                    COMPLETE GBA EMULATOR - PURE PYTHON EDITION                         ║")
    print("║                         Team Flames / Samsoft / Cat OS                                 ║")
    print("╠════════════════════════════════════════════════════════════════════════════════════════╣")
    print("║  ARM7TDMI CPU with full ARM and Thumb instruction sets                                 ║")
    print("║  All video modes (0-5) with backgrounds and sprites                                    ║")
    print("║  DMA, Timers, and Interrupt support                                                    ║")
    print("╠════════════════════════════════════════════════════════════════════════════════════════╣")
    print("║  Controls: Arrows=D-Pad  Z=A  X=B  Enter=Start  Backspace=Select  A=L  S=R             ║")
    print("╚════════════════════════════════════════════════════════════════════════════════════════╝")
    
    root = tk.Tk()
    app = EmulatorUI(root)
    root.mainloop()
