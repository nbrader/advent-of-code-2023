import pygame
import math

# Python translations of Haskell functions
def enum_signed(z):
    absZ = z + 1
    signZ = absZ & 1
    i = (absZ >> 1) * (-1 if signZ == 0 else 1)
    return i

def inverse_enum_signed(i):
    return -2 * i if i <= 0 else 2 * i - 1

def pairing(i, j):
    if i == 0 and j == 0:
        return 0
    i0 = i & 1
    j0 = j & 1
    next_pair = pairing(i >> 1, j >> 1)
    return (i0 | (j0 << 1)) | (next_pair << 2)

def unpairing(z):
    def unpair(n, bit, i, j):
        if n == 0:
            return i, j
        i_bit = n & 1
        j_bit = (n >> 1) & 1
        new_i = i | (i_bit << bit)
        new_j = j | (j_bit << bit)
        return unpair(n >> 2, bit + 1, new_i, new_j)

    return unpair(z, 0, 0, 0)

def signed_pairing(i, j):
    return pairing(inverse_enum_signed(i), inverse_enum_signed(j))

def signed_unpairing(z):
    i, j = unpairing(z)
    return enum_signed(i), enum_signed(j)

# Function to scale and translate points to fit in the window
def transform_point(x, y, width, height):
    scale = 2  # Scale factor for better visualization
    return width // 2 + x * scale, height // 2 - y * scale

# Function to get a changing color
def get_color(index):
    r = int((math.sin(index * 0.001) + 1) * 127.5)
    g = int((math.sin(index * 0.001 + 2) + 1) * 127.5)
    b = int((math.sin(index * 0.001 + 4) + 1) * 127.5)
    return (r, g, b)

def main():
    # Initialize Pygame
    pygame.init()

    # Window settings
    width, height = 1024, 1024
    screen = pygame.display.set_mode((width, height))
    pygame.display.set_caption('Point Visualization')

    # Colors
    white = (255, 255, 255)

    # # Set up clock for timing
    # clock = pygame.time.Clock()

    # List to store points
    points = []

    # Main loop
    running = True
    last_point = None
    point_index = 0
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        # Check if we need to add a new point
        decoded_x, decoded_y = signed_unpairing(point_index)
        point = transform_point(decoded_x, decoded_y, width, height)
        points.append(point)
        point_index += 1
        
        line_color = get_color(point_index)  # Get the changing color for the line
        # Draw the latest point and line
        # if last_point:
        #     pygame.draw.line(screen, line_color, last_point, points[-1], 2)
        pygame.draw.circle(screen, ((point_index-1)*(point_index+1)) % (256*256*256), points[-1], 1)  # Draw the latest point

        last_point = points[-1]

        # Update display
        pygame.display.flip()

        # # Adjust the speed of point addition
        # clock.tick(1000000000000000000000000000)

    pygame.quit()

main()
