/**
 *
 */
package message_passing;

import java.util.Scanner;

/**
 * @author Andrew
 *
 */
public class InputComponent extends Component
{
    private final Client c;

    public InputComponent(Client client) {
        this.c = client;
    }

    @Override
    public void send(IMessage message) {
        c.addMessage(message);
    }

    @Override
    public void start() {
        final InputComponent self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                String input = null;
                Scanner scanner = new Scanner(System.in);
                do {
                    System.out.print("\nEnter a message (or 'q' to quit): ");
                    input = scanner.nextLine();
                    send(new TextMessage(self, input));
                } while (!"q".equalsIgnoreCase(input));
                scanner.close();
            }
        }).start();
    }

    @Override
    public void handle(TextMessage msg)
    {
        System.out.println("handling message " + msg.getText());
    }
}
