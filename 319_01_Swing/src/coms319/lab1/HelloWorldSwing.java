package coms319.lab1;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

public class HelloWorldSwing {
    private static JTextField inputField;
    private static JRadioButton rdbtn_Bold, rdbtn_Italic, rdbtn_Bold_Italic, rdbtn_Plain;
    private static JLabel echoInput;
    private static JComboBox<String> comboBox;

    public static void main(String[] args) {
        // Create main frame and properties
        final JFrame mainFrame = new JFrame("HelloWorldSwing!");
        mainFrame.setSize(370, 220);
        mainFrame.getContentPane().setLayout(new BorderLayout());

        // Construct North Panel
        final JPanel northPanel = new JPanel();
        mainFrame.add(northPanel, BorderLayout.NORTH);
        northPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
        FontListener fl = new FontListener();

        JLabel text = new JLabel("Text:");
        northPanel.add(text);

        // Create text input field
        inputField = new JTextField();
        inputField.addKeyListener(new KeyAdapter() {
            // Need to use keyReleased instead of keyPressed because
            // the inputField.getText() does not get updated until 
            // after the keyTyped keyListener
            @Override
            public void keyReleased(KeyEvent e) {
                echoInput.setText(inputField.getText());
            }
        });
        inputField.setText("Hello World!");
        northPanel.add(inputField);
        inputField.setColumns(10);

        comboBox = new JComboBox<String>();
        comboBox.setModel(new DefaultComboBoxModel<String>(new String[] { "Tiny", "Small", "Medium", "Large" }));
        comboBox.addActionListener(fl);
        northPanel.add(comboBox);

        // Middle panel
        JPanel westPanel = new JPanel();
        mainFrame.getContentPane().add(westPanel, BorderLayout.WEST);
        westPanel.setLayout(new GridLayout(0, 1, 0, 0));

        ButtonGroup rdbtn_group = new ButtonGroup();
        rdbtn_Plain = new JRadioButton("Plain");
        rdbtn_Bold = new JRadioButton("Bold");
        rdbtn_Italic = new JRadioButton("Italic");
        rdbtn_Bold_Italic = new JRadioButton("Bold Italic");

        rdbtn_Plain.addActionListener(fl);
        rdbtn_Plain.setSelected(true);
        rdbtn_Bold.addActionListener(fl);
        rdbtn_Italic.addActionListener(fl);
        rdbtn_Bold_Italic.addActionListener(fl);

        rdbtn_group.add(rdbtn_Plain);
        rdbtn_group.add(rdbtn_Bold);
        rdbtn_group.add(rdbtn_Italic);
        rdbtn_group.add(rdbtn_Bold_Italic);
        westPanel.add(rdbtn_Plain);
        westPanel.add(rdbtn_Bold);
        westPanel.add(rdbtn_Italic);
        westPanel.add(rdbtn_Bold_Italic);

        // South panel
        JPanel southPanel = new JPanel();
        mainFrame.getContentPane().add(southPanel, BorderLayout.SOUTH);

        JButton btnExit = new JButton("Exit");
        btnExit.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                mainFrame.dispose();
            }
        });

        JLabel spacer = new JLabel("                          ");

        JPanel eastPanel = new JPanel();
        mainFrame.getContentPane().add(eastPanel, BorderLayout.CENTER);
        eastPanel.setLayout(new GridLayout(0, 1, 0, 0));

        echoInput = new JLabel("Hello World!");
        echoInput.setHorizontalAlignment(SwingConstants.CENTER);
        echoInput.setForeground(Color.black);
        eastPanel.add(echoInput);

        JButton btnShow = new JButton("Show!");
        btnShow.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                echoInput.setForeground(echoInput.getForeground() == Color.black ? Color.red : Color.black);
            }
        });
        southPanel.add(btnShow);
        southPanel.add(spacer);
        southPanel.add(btnExit);

        mainFrame.setVisible(true);
    }

    private static class FontListener implements ActionListener
    {
        @Override
        public void actionPerformed(ActionEvent event)
        {
            Object source = event.getSource();

            if (source == rdbtn_Bold)
                echoInput.setFont(echoInput.getFont().deriveFont(Font.BOLD));
            else if (source == rdbtn_Italic)
                echoInput.setFont(echoInput.getFont().deriveFont(Font.ITALIC));
            else if (source == rdbtn_Plain)
                echoInput.setFont(echoInput.getFont().deriveFont(Font.PLAIN));
            else if (source == rdbtn_Bold_Italic)
                echoInput.setFont(echoInput.getFont().deriveFont(Font.BOLD + Font.ITALIC));
            //8 12 20 28
            else if (source == comboBox) {
                String fontSize = ((JComboBox<String>) source).getSelectedItem().toString();
                if ("Tiny".equalsIgnoreCase(fontSize))
                    echoInput.setFont(echoInput.getFont().deriveFont(8.0f));
                else if ("Small".equalsIgnoreCase(fontSize))
                    echoInput.setFont(echoInput.getFont().deriveFont(12.0f));
                else if ("Medium".equalsIgnoreCase(fontSize))
                    echoInput.setFont(echoInput.getFont().deriveFont(20.0f));
                else if ("Large".equalsIgnoreCase(fontSize))
                    echoInput.setFont(echoInput.getFont().deriveFont(28.0f));
            }
            else
                throw new RuntimeException("Unknown source: " + source);
        }
    }
}
