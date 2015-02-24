package function_plotter;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Scanner;

/**
 * Try out the FunctionPlotter class.
 */
public class FunctionPlotterMain
{
    public static void main(String[] args)
    {
        FunctionPlotter calc = new FunctionPlotter(0.1);

        // A variable of type SVFunction can refer to 
        // any subtype (that is, any class that implements the 
        // SingleVariableFunction interface).
        SVFunction f = null;

        Scanner scanner = new Scanner(System.in);
        String s = getEntry(scanner);

        while (!s.equals("q"))
        {
            if (s.equals("a"))
            {
                f = new LinearFunction(1.0, 0.0);
            }
            else if (s.equals("b"))
            {
                f = new QuadraticFunction(1.0, 0.0, 0.0);
            }
            else if (s.equals("c"))
            {
                f = new SineFunction(3.0, 2.0);
            }
            else if (s.equals("d"))
            {
                // OPTIONAL: we can even read in a new class defined at runtime!
                // Try it with the class good.Foo (constructor has 0 parameters)
                System.out.print("Enter class name (including package): ");
                String className = scanner.next();
                System.out.print("How many double parameters in the constructor? ");
                int numParams = scanner.nextInt();
                f = createFromName(className, numParams);
            }
            else
            {
                System.out.println("Enter a, b, c, d (or q to quit)");
            }
            if (f != null)
            {
                // Now whatever f refers to, we can plot it
                calc.plotFunction(f);
            }

            s = getEntry(scanner);
        }
    }

    /**
     * Prints a menu and gets the user's choice.
     * 
     * @param scanner
     *            Scanner to use for reading from the console
     * @return
     *         letter entered by the user
     */
    private static String getEntry(Scanner scanner)
    {
        System.out.println("What kind of function do you want to plot? (q to quit)");
        System.out.println("a) Linear function");
        System.out.println("b) Quadratic function");
        System.out.println("c) Sine function");
        System.out.println("d) Enter your function name");
        System.out.print("Your choice: ");
        return scanner.next();
    }

    /**
     * OPTIONAL: This example for the ambitious only! This method uses
     * some Java magic to create a new SingleVariableFunction
     * instance from a given class name. The class must implement
     * the SingleVariableFunction interface and it must have a constructor
     * that has zero or more double parameters. Returns null if something
     * goes wrong and the instance can't be constructed.
     * 
     * @param className
     *            name of the class implementing the SingleVariableFunction interface
     * @param numParams
     *            number of double parameters required by the constructor
     * @return
     *         a SingleVariableFunction instance of the type given by 'className'
     */
    private static SVFunction createFromName(String className, int numParams)
    {
        SVFunction ret = null;
        try
        {
            // creates a class from the name
            Class<?> c = Class.forName(className);

            // find a constructor
            Constructor<?>[] ctors = c.getConstructors();
            for (Constructor<?> ctor : ctors)
            {
                if (ctor.getParameterTypes().length == numParams)
                {
                    // invoke the constructor to create an instance of the class
                    ret = (SVFunction) ctor.newInstance();
                    break;
                }
            }
            if (ret == null)
            {
                System.out.println("Unable to find a constructor with " + numParams + " parameters");
            }
        } catch (ClassNotFoundException e)
        {
            System.out.println("Unable to find class " + className + ":" + e.toString());
        } catch (InstantiationException e)
        {
            System.out.println("Unable to create class " + className + " using a constructor with " + numParams + " double parameters: " + e.toString());
        } catch (InvocationTargetException e)
        {
            System.out.println("Unable to create class " + className + " using a constructor with " + numParams + " double parameters: " + e.toString());
        } catch (IllegalAccessException e)
        {
            System.out.println("Unable to create class " + className + " using a constructor with " + numParams + " double parameters: " + e.toString());
        }
        return ret;
    }
}