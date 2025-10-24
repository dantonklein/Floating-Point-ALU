import math
import struct

def float_to_bits(f):
    # Convert float to 32-bit representation
    return struct.unpack('>I', struct.pack('>f', f))[0]

def bits_to_float(bits):
    # Convert 32-bit representation to float
    return struct.unpack('>f', struct.pack('>I', bits))[0]

def generate_reciprocal_lut():
    # Generate 256-entry reciprocal lookup table
    lut = []
    
    for i in range(256):
        # Create mantissa value
        mantissa_bits = i << 15
        float_bits = (127 << 23) | mantissa_bits
        input_value = bits_to_float(float_bits)
        
        # Calculate reciprocal
        reciprocal = 1.0 / input_value
        reciprocal_bits = float_to_bits(reciprocal)
        
        # Extract exponent and mantissa
        reciprocal_exp = (reciprocal_bits >> 23) & 0xFF
        reciprocal_mantissa = reciprocal_bits & 0x7FFFFF
        
        # Denormalize to Q1.23 fixed point
        # The value is: (1.mantissa) × 2^(exp - 127)
        # We want it in Q1.23 where bit 23 is the 2^0 place
        
        # Add implicit 1 to get actual mantissa value
        reciprocal_mantissa_with_implicit = 0x800000 | reciprocal_mantissa  # 1.mantissa
        
        # Shift based on exponent
        # If exp = 127 (2^0), no shift needed
        # If exp = 126 (2^-1), shift right by 1
        # If exp = 125 (2^-2), shift right by 2
        shift = 127 - reciprocal_exp
        
        if shift > 0:
            reciprocal_fixed = reciprocal_mantissa_with_implicit >> shift
        elif shift < 0:
            reciprocal_fixed = reciprocal_mantissa_with_implicit << (-shift)
        else:
            reciprocal_fixed = reciprocal_mantissa_with_implicit
        
        # Keep only 24 bits for Q1.23
        recip_fixed = reciprocal_fixed & 0xFFFFFF
        # Store full 24-bit mantissa (including implicit 1)
        lut.append(reciprocal_fixed)
        
    return lut

lut = generate_reciprocal_lut()

with open('reciprocal_lut256.mem', 'w') as f:
    for value in lut:
        f.write(f'{value:06X}\n')