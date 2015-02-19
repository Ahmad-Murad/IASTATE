package sample;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;

public class JDBCMain
{
    // url format is:  jdbc:<driver>:<dbName>;<anyProperties>
    // create=true will create the database if it doesn't already exist
    public static final String DB_URL = "jdbc:derby:derbyDB;create=true";
    public static final String DB_DRIVER = "org.apache.derby.jdbc.EmbeddedDriver"; // full name of derby driver class

    public static void main(String args[]) throws Exception
    {
        Connection conn = DriverManager.getConnection(DB_URL);

        Statement stmt = conn.createStatement();
        stmt.execute("CREATE TABLE MYTABLE (COLOR_ID INT NOT NULL, COLOR VARCHAR(20))"); // only do this once

        // put some data in the table
        stmt.execute("INSERT INTO MYTABLE VALUES (1, 'BLUE')");
        stmt.execute("INSERT INTO MYTABLE VALUES (2, 'RED')");
        stmt.execute("INSERT INTO MYTABLE VALUES (3, 'GREEN')");

        // select and print data in the table
        ResultSet rs = stmt.executeQuery("SELECT * FROM MYTABLE");
        while (rs.next())
            System.out.println("Got entry with color_id=" + rs.getInt("COLOR_ID") + "   color=" + rs.getString("COLOR"));

        // always try to close resources
        rs.close();
        stmt.close();
        conn.close();
    }
}
