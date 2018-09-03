from sympy import *

x = symbols('x')
y = symbols('y')
print(x + x + y)
print(x - x)
print((x ** 2 + 2 * x + 4).subs(x, y))
print(((x + 2) * (x - 2)).expand())

