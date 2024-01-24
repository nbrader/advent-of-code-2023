import pygame
import time
import math

# Python translations of Haskell functions
def enum_pair_unsigned(x, y):
    return ((x + y + 1) * (x + y) // 2) + y

def enum_pair_unsigned_inv(n):
    w = int(((8 * n + 1) ** 0.5 - 1) / 2)
    t = (w ** 2 + w) // 2
    y = n - t
    x = w - y
    return x, y

def enum_signed(n):
    return 2 * (n - 1) + 1 if n > 0 else -2 * n

def enum_signed_inv(n):
    q, r = divmod(n + 1, 2)
    return q if r == 0 else -q

def enum_pair_signed(x, y):
    return enum_pair_unsigned(enum_signed(x), enum_signed(y))

def alternate(n):
    return -1 if n % 2 == 0 else 1

def enum_pair_signed_inv(n):
    if n == 0:
        return 0, 0

    x = int(math.floor(math.sqrt((n - 1) / 2 + 1) - 1))
    i = int((n - 1) // 2) - ((x + 1) ** 2 - 1)
    q = 2 * (x + 1) + 1

    alt11 = alternate(n)
    alt12 = alternate(int(math.floor(math.sqrt((n - 1) / 2 + 1))))
    alt22 = alternate(int(math.floor(math.sqrt((n - 1) / 2 + 5 / 4) - 1 / 2)))

    return (
        alt11 * alt12 * (i if (i // ((q + 1) >> 1)) == 0 else q - i),
        -alt11 * alt22 * abs((x + 1) - i)
    )

# Function to scale and translate points to fit in the window
def transform_point(x, y, width, height):
    scale = 4  # Scale factor for better visualization
    return width // 2 + x * scale, height // 2 - y * scale

# Function to get a changing color
def get_color(index):
    r = int((math.sin(index * 0.05) + 1) * 127.5)
    g = int((math.sin(index * 0.05 + 2) + 1) * 127.5)
    b = int((math.sin(index * 0.05 + 4) + 1) * 127.5)
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

    # Set up clock for timing
    clock = pygame.time.Clock()

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
        decoded_x, decoded_y = enum_pair_signed_inv(point_index)
        point = transform_point(decoded_x, decoded_y, width, height)
        points.append(point)
        point_index += 1
        print(point_index)

        # Draw the latest point and line
        # if last_point:
            # line_color = get_color(point_index)  # Get the changing color for the line
            # pygame.draw.line(screen, line_color, last_point, points[-1], 2)
        pygame.draw.circle(screen, white, points[-1], 1)  # Draw the latest point

        last_point = points[-1]

        # Update display
        pygame.display.flip()

        # Adjust the speed of point addition
        clock.tick(100000000)

    pygame.quit()

main()