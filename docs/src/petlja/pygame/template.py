from pygame import open_window, draw, Color
from pygamebg import wait_loop

(sirina, visina) = (400, 400)
prozor = open_window(sirina, visina, 'Pygame')
prozor.fill(Color('white'))
draw.line(prozor, Color('black'), (100, 100), (300, 300), 5)
wait_loop()
