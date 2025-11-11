import math
import struct

def float_to_bits(f):
    # Convert float to 32-bit representation
    return struct.unpack('>I', struct.pack('>f', f))[0]

def bits_to_float(bits):
    # Convert 32-bit representation to float
    return struct.unpack('>f', struct.pack('>I', bits))[0]

def generate_rsqrt_lut_normal():
    # Generate 256-entry inverse square root lookup table
    lut_normal = []
    
    for i in range(256):
        # Create mantissa value
        mantissa_bits = i << 15
        float_bits = (127 << 23) | mantissa_bits
        input_value = bits_to_float(float_bits)

        rsqrt_normal = 1.0 / math.sqrt(input_value)

        out_normal = int(rsqrt_normal * (1 << 23)) & 0xFFFFFF
    
        lut_normal.append(out_normal)
    
    return lut_normal

def generate_rsqrt_lut_double():
    # Generate 256-entry inverse square root lookup table
    lut_normal = []
    lut_double = []
    
    for i in range(256):
        # Create mantissa value
        mantissa_bits = i << 15
        float_bits = (127 << 23) | mantissa_bits
        input_value = bits_to_float(float_bits)

        rsqrt_double = 1.0 / math.sqrt(2 * input_value)

        out_double = int(rsqrt_double * (1 << 23)) & 0xFFFFFF
    
        lut_double.append(out_double)
    
    return lut_double
# Generate single LUT for [1, 4) range
lut_normal = generate_rsqrt_lut_normal()
lut_double = generate_rsqrt_lut_double()

with open('inverse_sqrt_lut256_normal.mem', 'w') as f:
    for value in lut_normal:
        f.write(f'{value:06X}\n')

with open('inverse_sqrt_lut256_double.mem', 'w') as f:
    for value in lut_double:
        f.write(f'{value:06X}\n')
