"""
################################################################################
#                                   LICENSE                                    #
################################################################################
#   This file is part of newtonian_black_holes.                                #
#                                                                              #
#   newtonian_black_holes is free software: you can redistribute it and/or     #
#   modify it under the terms of the GNU General Public License as published   #
#   by the Free Software Foundation, either version 3 of the License, or       #
#   (at your option) any later version.                                        #
#                                                                              #
#   newtonian_black_holes is distributed in the hope that it will be useful,   #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with newtonian_black_holes.  If not, see                             #
#   <https://www.gnu.org/licenses/>.                                           #
################################################################################
#   Purpose:                                                                   #
#       Basic test of raytracing routines. Generates a single black hole.      #
################################################################################
#   Author:     Ryan Maguire                                                   #
#   Date:       July 26, 2023.                                                 #
################################################################################
"""

# All raytracing routines are provided here.
import nbh

# Chosen parameters for the experiment.
force = nbh.setup.gravity
stopper = nbh.setup.stop
colorer = nbh.color.checker_board
solver = nbh.euler.path

# Run the raytracer.
nbh.run.run(force, stopper, colorer, solver, "newtonian_black_hole.ppm")
