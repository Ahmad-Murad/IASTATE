import java.awt.Dimension;
import javax.swing.JFrame;

/**
 * Here we just get started with Java Swing.
 * First let's play with JFrame - which is the main window created
 * when we run a typical Java Swing program.
 * 
 * @author smitra
 *
 */
public class BasicGUI01 {

	public static void main(String[] args) {
		
		// This is a HEAVYWEIGHT CONTAINER!
		JFrame frame = new JFrame("Frame Title - Andrew");
		
		// shape of the frame.
	    frame.setSize(new Dimension(600, 600));
	    
	    // You will find that the window vanishes but the program does not exit!
	    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    
	    // You will find that the window gets "packed" and vanishes (because it has nothing in it).
	    // Only the controls are visible and you can exit by clicking on the x.
	    //frame.pack();
	    
	    frame.setVisible(true);
	}

}
