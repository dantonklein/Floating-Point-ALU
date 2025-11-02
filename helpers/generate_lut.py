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
        # Convert directly to Q1.26 fixed point (27 bits)
        # Q1.26 means: 1 integer bit, 26 fractional bits
        # Multiply by 2^26 to convert to fixed-point
        fixed_point_27bit = int(reciprocal * (1 << 26))
        
        # Clamp to 27 bits (shouldn't be necessary but safe)
        fixed_point_27bit = fixed_point_27bit & 0x7FFFFFF
        
        lut.append(fixed_point_27bit)
        
    return lut

lut = generate_reciprocal_lut()

with open('reciprocal_lut256.mem', 'w') as f:
    for value in lut:
        f.write(f'{value:06X}\n')