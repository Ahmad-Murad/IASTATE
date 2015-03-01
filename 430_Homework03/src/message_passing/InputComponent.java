package message_passing;

import java.util.Scanner;

/**
 * @author Andrew
 *
 */
public class InputComponent extends Component
{
    private final Component c;

    public InputComponent(Component client) {
        this.c = client;
    }

    @Override
    public void send(IMessage message) {
        message.dispatch(this);
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
                    System.out.println("Please enter 'd', 'q', or an id number");
                    input = scanner.nextLine();
                    handle(new TextMessage(self, input));
                } while (!"q".equalsIgnoreCase(input));
                scanner.close();
            }
        }, "Input Component Thread").start();
    }

    @Override
    public void handle(TextMessage msg) {
        c.send(msg);
    }
}
