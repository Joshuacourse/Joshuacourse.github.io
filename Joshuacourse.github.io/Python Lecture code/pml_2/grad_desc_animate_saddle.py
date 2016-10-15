"""
A simple example of an animated plot
"""
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import matplotlib.animation as animation

def func(x,y):
    return (y**2-x**2)

def func_grad(x,y):
    return (-2*x, 2*y)

fig  = plt.figure()
ax = fig.gca(projection='3d', elev=35., azim=-30)
X, Y = np.meshgrid(np.arange(-6, 6, 0.25), np.arange(-6, 6, 0.25))
Z = func(X,Y) 

def init():
    c='r'
    ax.plot_surface(X, Y, Z, rstride=1, cstride=1, 
                    cmap=cm.coolwarm, linewidth=0.1, alpha=0.3)
    ax.set_zlim(-50, 50)
    ax.scatter(0.001, 4, func(0.001, 4), c=c, marker='o' )
    return ax

def animate(i):
    c='r'; xt = 0.001; yt = 4; eta=0.3
    for j in range(i):
        gx, gy = func_grad(xt, yt)
        xt = xt - eta*gx
        yt = yt - eta*gy

 
    ax.scatter(xt, yt, func(xt, yt), c=c, marker='o' )
    return ax

ani = animation.FuncAnimation(fig, animate, np.arange(19), init_func=init,
                              interval=200, blit=False, repeat=False)
plt.show()