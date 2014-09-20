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

public class HelloWorldSwing2 {
    private static JTextField txtHelloWorld;
    private static JRadioButton rdbtnNewRadioButton, rdbtnNewRadioButton_1, rdbtnNewRadioButton_2, rdbtnNewRadioButton_3;
    private static JComboBox comboBox;
    private static JLabel lblNewLabel_1;

    public static void main(String[] args) {
        final JFrame mainFrame = new JFrame("HelloWorldSwing!");
        mainFrame.setSize(370, 220);
        mainFrame.getContentPane().setLayout(new BorderLayout());

        final JPanel panel_1 = new JPanel();
        mainFrame.getContentPane().add(panel_1, BorderLayout.NORTH);
        panel_1.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));

        JLabel lblNewLabel = new JLabel("Text:");
        panel_1.add(lblNewLabel);

        txtHelloWorld = new JTextField();
        txtHelloWorld.setText("Hello World!");
        panel_1.add(txtHelloWorld);
        txtHelloWorld.setColumns(10);

        FontListener fl = new FontListener();
        comboBox = new JComboBox();
        comboBox.setModel(new DefaultComboBoxModel(new String[] { "Tiny", "Small", "Medium", "Large" }));
        comboBox.addActionListener(fl);
        panel_1.add(comboBox);

        JPanel panel_2 = new JPanel();
        mainFrame.getContentPane().add(panel_2, BorderLayout.WEST);
        panel_2.setLayout(new GridLayout(0, 1, 0, 0));

        rdbtnNewRadioButton = new JRadioButton("Plain");
        rdbtnNewRadioButton.addActionListener(fl);
        rdbtnNewRadioButton.setSelected(true);
        panel_2.add(rdbtnNewRadioButton);

        rdbtnNewRadioButton_1 = new JRadioButton("Bold");
        rdbtnNewRadioButton_1.addActionListener(fl);
        panel_2.add(rdbtnNewRadioButton_1);

        rdbtnNewRadioButton_2 = new JRadioButton("Italic");
        rdbtnNewRadioButton_2.addActionListener(fl);
        panel_2.add(rdbtnNewRadioButton_2);

        rdbtnNewRadioButton_3 = new JRadioButton("Bold Italic");
        rdbtnNewRadioButton_3.addActionListener(fl);
        panel_2.add(rdbtnNewRadioButton_3);

        ButtonGroup btnGroup = new ButtonGroup();
        btnGroup.add(rdbtnNewRadioButton);
        btnGroup.add(rdbtnNewRadioButton_1);
        btnGroup.add(rdbtnNewRadioButton_2);
        btnGroup.add(rdbtnNewRadioButton_3);

        JPanel panel_3 = new JPanel();
        mainFrame.getContentPane().add(panel_3, BorderLayout.SOUTH);

        JPanel panel_4 = new JPanel();
        mainFrame.getContentPane().add(panel_4, BorderLayout.CENTER);
        panel_4.setLayout(new GridLayout(0, 1, 0, 0));

        lblNewLabel_1 = new JLabel("Hello World!");
        lblNewLabel_1.setHorizontalAlignment(SwingConstants.CENTER);
        lblNewLabel_1.setForeground(Color.black);
        txtHelloWorld.addKeyListener(new KeyAdapter() {
            // Need to use keyReleased instead of keyPressed because
            // the inputField.getText() does not get updated until 
            // after the keyTyped keyListener
            @Override
            public void keyReleased(KeyEvent e) {
                lblNewLabel_1.setText(txtHelloWorld.getText());
            }
        });
        panel_4.add(lblNewLabel_1);

        JButton btnShow = new JButton("Show!");
        btnShow.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                lblNewLabel_1.setForeground(lblNewLabel_1.getForeground() == Color.black ? Color.red : Color.black);
            }
        });
        panel_3.add(btnShow);

        JLabel label = new JLabel("                            ");
        panel_3.add(label);

        JButton btnExit = new JButton("Exit");
        btnExit.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                mainFrame.dispose();
            }
        });
        panel_3.add(btnExit);

        mainFrame.setVisible(true);
    }

    private static class FontListener implements ActionListener
    {
        @Override
        public void actionPerformed(ActionEvent event)
        {
            Object source = event.getSource();

            if (source == rdbtnNewRadioButton_1)
                lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(Font.BOLD));
            else if (source == rdbtnNewRadioButton_2)
                lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(Font.ITALIC));
            else if (source == rdbtnNewRadioButton)
                lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(Font.PLAIN));
            else if (source == rdbtnNewRadioButton_3)
                lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(Font.BOLD + Font.ITALIC));
            //8 12 20 28
            else if (source == comboBox) {
                String fontSize = ((JComboBox<String>) source).getSelectedItem().toString();
                if ("Tiny".equalsIgnoreCase(fontSize))
                    lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(8.0f));
                else if ("Small".equalsIgnoreCase(fontSize))
                    lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(12.0f));
                else if ("Medium".equalsIgnoreCase(fontSize))
                    lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(20.0f));
                else if ("Large".equalsIgnoreCase(fontSize))
                    lblNewLabel_1.setFont(lblNewLabel_1.getFont().deriveFont(28.0f));
            }
            else
                throw new RuntimeException("Unknown source: " + source);
        }
    }
}
