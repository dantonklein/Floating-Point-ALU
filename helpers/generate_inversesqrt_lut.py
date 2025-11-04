import math
import struct

def float_to_bits(f):
    # Convert float to 32-bit representation
    return struct.unpack('>I', struct.pack('>f', f))[0]

def bits_to_float(bits):
    # Convert 32-bit representation to float
    return struct.unpack('>f', struct.pack('>I', bits))[0]

def generate_rsqrt_lut():
    # Generate 256-entry inverse square root lookup table
    # Input: Q2.23 mantissa after pre-scaling (covers [1.0, 4.0))
    # Output: Q2.26 fixed point
    lut = []
    
    for i in range(256):
        # Map index to input range [1.0, 4.0)
        # Use top 8 bits of the Q2.23 input
        # Input range spans 2 integer bits: [1.0, 4.0)
        input_value = 1.0 + (i * 3.0 / 256.0)  # Maps 0-255 to [1.0, 3.996)
        
        # Calculate 1/sqrt(input_value)
        rsqrt = 1.0 / math.sqrt(input_value)
        
        # Convert to Q2.26 fixed point (28 bits total)
        # Q2.26 means: 2 integer bits, 26 fractional bits
        # Multiply by 2^26 to convert to fixed-point
        fixed_point_28bit = int(rsqrt * (1 << 26))
        
        # Clamp to 28 bits
        fixed_point_28bit = fixed_point_28bit & 0xFFFFFFF
        
        lut.append(fixed_point_28bit)
        
    return lut
# Generate single LUT for [1, 4) range
lut = generate_rsqrt_lut()

with open('inverse_sqrt_lut256.mem', 'w') as f:
    for value in lut:
        f.write(f'{value:07X}\n')
