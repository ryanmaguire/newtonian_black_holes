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
package nbh;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class PPM {
    public File file;
    public FileWriter writer;

    public PPM(String filename) {
        try {
            file = new File(filename);
            file.createNewFile();
            writer = new FileWriter(filename);
        } catch (IOException e) {
            System.out.println("Error occurred opening PPM file.");
            e.printStackTrace();
        }
    }

    public void initFromValues(int x, int y, int type) {
        try {
            writer.write(String.format("P%d%n%d %d%n255%n", type, x, y));
        } catch (IOException e) {
            System.out.println("Error occurred initializing PPM.");
            e.printStackTrace();
        }
    }

    public void init() {
        initFromValues(Setup.x_size, Setup.y_size, 3);
    }

    public void write(String string) {
        try {
            writer.write(string);
        } catch (IOException e) {
            System.out.println("Error occurred writing to PPM.");
            e.printStackTrace();
        }
    }

    public void close() {
        try {
            writer.close();
        } catch (IOException e) {
            System.out.println("Error occurred closing PPM.");
            e.printStackTrace();
        }
    }
}
