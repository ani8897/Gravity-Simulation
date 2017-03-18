# Gravity-Simulation

We turn to the problem of simulating the physical interaction due to gravitation between
a number of particles in space and over a period of time. While we say particles, we mean
objects of any size that can be in
uenced by gravitational forces. For simplicity, let us
restrict ourselves to two dimensions. The particles have a mass, an initial velocity and an
initial position in the region. Then, from elementary physics, you can find out the force
on each of the particles. This enables you to find the new position and the new velocity
after a small time delta_t. This process can be repeated as many times as required.

This is fine except the computation of this process may take too long. The number of
particles may be too large if one is simulating, for instance, the evolution of a galaxy.
If the number of particles is n, then the force on any one of the particles is calculated
by considering the other n - 1 particles. Thus the computation at each time step takes
O(n2) time. Can we reduce this time using a combination of data structures and the
physics of the situation involved?

It turns out that a simplification can be achieved by the following principle: Consider
a particle p and a group of particles G. If all the particles in G are in a region that
is "sufficiently far away from p" in a well-defined sense, then the particles in G can be
approximated by a single particle at its centroid. This is the idea that we shall use.

Assume that all the particles are within a square region. We now group the particles as follows: If a square has more than one particle, divide it
into four squares of equal size. Repeat this process recursively till each square has at most
one particle (a quadrant may be empty) If the particle
falls on the horizontal dividing line, choose the upper quadrant. If the particle falls on the
vertical dividing line, choose the quadrant on the right. We can think of the interior nodes in the trees
as also being particles. The mass of a particle at an interior node is the sum of the
particles represented by its sub-trees. Its position is the centroid of masses of the particles
represented by the sub-trees of the node.
