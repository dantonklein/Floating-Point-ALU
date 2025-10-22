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
        # Create mantissa value: 1.xxxxxxxx (where x's are the 8 bits)
        # Mantissa in [1.0, 2.0)
        # We're using top 8 bits of 23-bit mantissa
        mantissa_bits = i << 15  # Shift to top 8 bits of 23-bit mantissa
        
        # Create a normalized float with exponent=127 (so value = 1.mantissa)
        float_bits = (127 << 23) | mantissa_bits
        input_value = bits_to_float(float_bits)
        
        # Calculate reciprocal
        reciprocal = 1.0 / input_value
        reciprocal_bits = float_to_bits(reciprocal)
        
        # Extract the mantissa (23 bits)
        if i == 0: 
            reciprocal_mantissa = 0x800000
        else:
            reciprocal_mantissa = reciprocal_bits & 0x7FFFFF
        
        # Store full 24-bit mantissa (including implicit 1)
        lut.append(reciprocal_mantissa)
        
    return lut

lut = generate_reciprocal_lut()

with open('reciprocal_lut256.mem', 'w') as f:
    for value in lut:
        f.write(f'{value:06X}\n')