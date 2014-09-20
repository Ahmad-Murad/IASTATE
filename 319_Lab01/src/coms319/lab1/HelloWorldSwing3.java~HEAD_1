package coms319.lab1;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.Timer;

public class HelloWorldSwing3 {
    private static JTextField inputField;
    private static JRadioButton rdbtn_Bold, rdbtn_Italic, rdbtn_Bold_Italic, rdbtn_Plain;
    private static JLabel echoInput;
    private static JComboBox<String> comboBox;
    private static JTextField clearAfterTime;
    private static int clearTime = -1;

    public static void main(String[] args) {
        // Create main frame and properties
        final JFrame mainFrame = new JFrame("HelloWorldSwing!");
        mainFrame.setSize(370, 220);
        mainFrame.getContentPane().setLayout(new BorderLayout());
        FontListener fl = new FontListener();

        ButtonGroup rdbtn_group = new ButtonGroup();

        JPanel mainPanel = new JPanel();
        mainFrame.getContentPane().add(mainPanel, BorderLayout.CENTER);
        mainPanel.setLayout(null);

        final JLabel errorMessageLabel = new JLabel("");
        errorMessageLabel.setBounds(80, 50, 264, 16);
        errorMessageLabel.setForeground(Color.red);
        mainPanel.add(errorMessageLabel);

        JLabel lblText = new JLabel("Text:");
        lblText.setBounds(6, 19, 32, 16);
        mainPanel.add(lblText);

        // Create text input field
        inputField = new JTextField();
        inputField.setBounds(44, 15, 74, 24);
        mainPanel.add(inputField);
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
        inputField.setColumns(10);
        rdbtn_Bold_Italic = new JRadioButton("Bold Italic");
        rdbtn_Bold_Italic.setBounds(13, 123, 108, 16);
        mainPanel.add(rdbtn_Bold_Italic);
        rdbtn_Bold_Italic.addActionListener(fl);
        rdbtn_group.add(rdbtn_Bold_Italic);
        rdbtn_Italic = new JRadioButton("Italic");
        rdbtn_Italic.setBounds(13, 78, 108, 16);
        mainPanel.add(rdbtn_Italic);
        rdbtn_Italic.addActionListener(fl);
        rdbtn_group.add(rdbtn_Italic);
        rdbtn_Bold = new JRadioButton("Bold");
        rdbtn_Bold.setBounds(13, 100, 108, 16);
        mainPanel.add(rdbtn_Bold);
        rdbtn_Bold.addActionListener(fl);
        rdbtn_group.add(rdbtn_Bold);
        rdbtn_Plain = new JRadioButton("Plain");
        rdbtn_Plain.setBounds(13, 58, 108, 16);
        mainPanel.add(rdbtn_Plain);

        rdbtn_Plain.addActionListener(fl);
        rdbtn_Plain.setSelected(true);

        rdbtn_group.add(rdbtn_Plain);

        final JCheckBox chkbox_clearInput = new JCheckBox("clear after:");
        chkbox_clearInput.setBounds(213, 19, 88, 16);
        mainPanel.add(chkbox_clearInput);

        comboBox = new JComboBox<String>();
        comboBox.setBounds(133, 17, 74, 23);
        mainPanel.add(comboBox);
        comboBox.setModel(new DefaultComboBoxModel<String>(new String[] { "Tiny", "Small", "Medium", "Large" }));

        JButton btnShow = new JButton("Show!");
        btnShow.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {}
        });
        btnShow.setBounds(23, 147, 107, 24);
        mainPanel.add(btnShow);

        JButton btnExit = new JButton("Exit");
        btnExit.setBounds(238, 147, 107, 24);
        mainPanel.add(btnExit);

        clearAfterTime = new JTextField();
        clearAfterTime.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                try {
                    String text = clearAfterTime.getText();
                    if (text == null || text.length() < 1)
                        return;
                    clearTime = Integer.valueOf(text);
                    if (clearTime < 1 || clearTime > 10)
                        throw new NumberFormatException();
                    errorMessageLabel.setText("");
                } catch (NumberFormatException ex) {
                    clearTime = -1;
                    errorMessageLabel.setText("Must enter a number between 1 and 10.");
                }
            }
        });
        clearAfterTime.setBounds(307, 13, 38, 28);
        mainPanel.add(clearAfterTime);
        clearAfterTime.setColumns(10);

        echoInput = new JLabel("Hello World!");
        echoInput.setBounds(133, 56, 212, 83);
        mainPanel.add(echoInput);
        echoInput.setHorizontalAlignment(SwingConstants.CENTER);
        echoInput.setForeground(Color.black);

        btnExit.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                mainFrame.dispose();
            }
        });
        btnShow.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                echoInput.setForeground(echoInput.getForeground() == Color.black ? Color.red : Color.black);
                if (chkbox_clearInput.isSelected() && clearTime != -1) {
                    final Timer t = new Timer(clearTime * 1000, null);
                    t.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(ActionEvent e) {
                            echoInput.setText("");
                            t.removeActionListener(this);
                        }
                    });
                    t.start();
                }
                echoInput.setText(inputField.getText());
            }
        });
        comboBox.addActionListener(fl);

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
