package apr07;

// Introduction to Software Testing
// Authors: Paul Ammann & Jeff Offutt
// Chapter 3, section 3.3, page 121

// Jeff Offutt--Java version Feb 2003
// Classify triangles
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/*
 * Example used for mutation testing
 */
class triangle {
    private static String[] triTypes = { "", // Ignore 0.
                                        "scalene", "isosceles", "equilateral", "not a valid triangle" };
    private static String instructions = "This is the ancient TriTyp program.\nEnter three integers that represent the lengths of the sides of a triangle.\nThe triangle will be categorized as either scalene, isosceles, equilateral\nor invalid.\n";

    public static void main(String[] argv) { // Driver program for trityp
        int A, B, C;
        int T;

        System.out.println(instructions);
        System.out.println("Enter side 1: ");
        A = getN();
        System.out.println("Enter side 2: ");
        B = getN();
        System.out.println("Enter side 3: ");
        C = getN();
        T = Triang(A, B, C);

        System.out.println("Result is: " + triTypes[T]);
    }

    // triOut is output from the routine:
    // Triang = 1 if triangle is scalene
    // Triang = 2 if triangle is isosceles
    // Triang = 3 if triangle is equilateral
    // Triang = 4 if not a triangle
    public static int Triang(int Side1, int Side2, int Side3) {
        int triOut = 0;
        if (Side1 <= 0 || Side2 <= 0 || Side3 <= 0) {
            return 4;
        }

        if (Side1 == Side2)
//        if (Side1 != Side2) //m1
            triOut = triOut + 1;
        if (Side1 == Side3)
            triOut = triOut + 2;
        if (Side2 == Side3)
            triOut = triOut + 3;
//            triOut = triOut - 3; //m2
        if (triOut == 0) {
            if (Side1 + Side2 <= Side3 || Side2 + Side3 <= Side1
                || Side1 + Side3 <= Side2)
                triOut = 4;
            else
                triOut = 1;
//                triOut -= 1; //m3
            return (triOut);
//            return (Side1); //m4
        }

        if (triOut > 3)
            triOut = 3;
        else if (triOut == 1 && Side1 + Side2 > Side3)
//        else if (triOut == 1 || Side1 + Side2 > Side3) //m5
            triOut = 2;
        else if (triOut == 2 && Side1 + Side3 > Side2)
            triOut = 2;
        else if (triOut == 3 && Side2 + Side3 > Side1)
            triOut = 2;
        else
            triOut = 4;
        return (triOut);
    } // end Triang

    // ====================================
    // Read (or choose) an integer
    private static int getN() {
        int inputInt = 1;
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String inStr;
        try {
            inStr = in.readLine();
            inputInt = Integer.parseInt(inStr);
        } catch (IOException e) {
            System.out.println("Could not read input, choosing 1.");
        } catch (NumberFormatException e) {
            System.out.println("Entry must be a number, choosing 1.");
        }
        return (inputInt);
    } // end getN

} // end trityp class