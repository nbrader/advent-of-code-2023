import pygame
import time

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

def enum_pair_signed_inv(n):
    x, y = enum_pair_unsigned_inv(n)
    return enum_signed_inv(x), enum_signed_inv(y)

# Function to scale and translate points to fit in the window
def transform_point(x, y, width, height):
    scale = 20  # Scale factor for better visualization
    return width // 2 + x * scale, height // 2 - y * scale

# Initialize Pygame
pygame.init()

# Window settings
width, height = 1024, 1024
screen = pygame.display.set_mode((width, height))
pygame.display.set_caption('Point Visualization')

# Colors
black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)

# Set up clock for timing
clock = pygame.time.Clock()

# Number of points to plot
num_points = 2000  # Adjust this number to display more or fewer points

# List to store points
points = []

# Main loop
running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Fill background
    screen.fill(black)

    # Check if we need to add a new point
    if len(points) < num_points:
        decoded_x, decoded_y = enum_pair_signed_inv(len(points))
        point = transform_point(decoded_x, decoded_y, width, height)
        points.append(point)

    # Draw points and lines
    last_point = None
    for point in points:
        pygame.draw.circle(screen, white, point, 5)
        if last_point:
            pygame.draw.line(screen, red, last_point, point, 2)
        last_point = point

    # Update display
    pygame.display.flip()

    # Wait for a second
    clock.tick(100)  # Adjust this number to change the speed of point addition

pygame.quit()