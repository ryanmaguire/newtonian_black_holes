/******************************************************************************
 *                                  LICENSE                                   *
 ******************************************************************************
 *  This file is part of newtonian_black_holes.                               *
 *                                                                            *
 *  newtonian_black_holes is free software: you can redistribute it and/or    *
 *  modify it under the terms of the GNU General Public License as published  *
 *  by the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  newtonian_black_holes is distributed in the hope that it will be useful   *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with newtonian_black_holes.  If not, see                            *
 *  <https://www.gnu.org/licenses/>.                                          *
 ******************************************************************************/

struct Vec2 {
    var x, y: Double
}

extension Vec2 {
    static func + (left: Vec2, right: Vec2) -> Vec2 {
        return Vec2(x: left.x + right.x, y: left.y + right.y)
    }

    static func - (left: Vec2, right: Vec2) -> Vec2 {
        return Vec2(x: left.x - right.x, y: left.y - right.y)
    }

    static func * (left: Double, right: Vec2) -> Vec2 {
        return Vec2(x: left*right.x, y: left*right.y)
    }
}
