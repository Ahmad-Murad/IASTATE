package coms319.lab2;

import java.awt.BorderLayout;
import java.awt.Desktop.Action;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;

/**
 * Simple example showing
 * 1) Menubar
 * 2) Toolbar
 * 3) Use of AbstractAction for same actions in menu, toolbar, and button
 * @author smitra
 *
 */
public class MenuExample {

	private static ExitAction exit;
	private static PrintHelloAction hello;

	public static void main(String[] args) {
		JFrame frame = new JFrame("Tree Frame");
		frame.setSize(400, 400);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(createMyPanel());

		// add a Menubar
		JMenuBar menubar = new JMenuBar();
		frame.setJMenuBar(menubar);

		// add a menu
		JMenu menu = new JMenu("File");
		menubar.add(menu);
		
		JMenu editMenu = new JMenu("Edit");
		menubar.add(editMenu);

		// add a menuitem!
		JMenuItem exitMenuItem = new JMenuItem(exit);
		menu.add(exitMenuItem);
		
		// add another menuitem
		JMenuItem printHelloMenuItem = new JMenuItem(hello);
		menu.add(printHelloMenuItem);
		
		// TODO: Add other Menu and Menu Items!
		
		frame.setVisible(true);

	}

	static JPanel createMyPanel() {
		JPanel myPanel = new JPanel();
		myPanel.setLayout(new BorderLayout());

		// Create a toolbar
		// Note that this is associated with a Panel and not a Frame.
		JToolBar toolbar = new JToolBar();
		myPanel.add(toolbar, BorderLayout.NORTH);

		// Add Actions to the tool bar.
		exit = new ExitAction("Exit", "Click to exit");
		hello = new PrintHelloAction("Print", "Click to print hello");
		toolbar.add(exit);
		toolbar.add(hello);

		// TODO: add other actions to the toolbar!

		// Add the main text area to the panel
		myPanel.add(new JScrollPane(new JTextArea("Hello This is ..")),
				BorderLayout.CENTER);

		// Create a bottom panel for buttons
		JPanel bottomPanel = new JPanel();
		myPanel.add(bottomPanel, BorderLayout.SOUTH);
		
		// Add buttons to the bottom panel
		JButton b1 = new JButton(exit);
		bottomPanel.add(b1);
		JButton b2 = new JButton(hello);
		bottomPanel.add(b2);

		return myPanel;
	}
}

// Useful when same action can be triggered from different places
// We played with use in Menu, Toolbar, and Button.
// We can set text, tooltip, icon etc;
// TODO: Create more classes that extend AbstractAction
class ExitAction extends AbstractAction {
	private static final long serialVersionUID = 1L;

	ExitAction(String name, String desc) {
		this.putValue(NAME, name);
		this.putValue(SHORT_DESCRIPTION, desc); // TOOL TIP
		ImageIcon myIcon = new ImageIcon("stop.gif");
		this.putValue(SMALL_ICON, myIcon);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		System.exit(0);
	}
}

//Useful when same action can be triggered from different places
//We played with use in Menu, Toolbar, and Button.
//We can set text, tooltip, icon etc;
class PrintHelloAction extends AbstractAction {
	private static final long serialVersionUID = 1L;

	PrintHelloAction(String name, String desc) {
		this.putValue(NAME, name);
		this.putValue(SHORT_DESCRIPTION, desc); // TOOL TIP
		ImageIcon myIcon = new ImageIcon("happyFace.gif");
		this.putValue(SMALL_ICON, myIcon);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		System.out.println("Hello!");
	}
}