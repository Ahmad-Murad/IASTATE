package server;

public class Server {
    public static void main(String[] args) {
        int status = 0;
        Ice.Communicator ic = null;
        try {
            // Server implementation here
            ic = Ice.Util.initialize(new String[] { "--Ice.ThreadPool.Server.Size=10" });
            Ice.ObjectAdapter adapter = ic.createObjectAdapterWithEndpoints("Simpledbexample", "default -p 10000");
            Ice.Object object = new DatabaseI();
            adapter.add(object, ic.stringToIdentity("Simpledbexample"));
            adapter.activate();
            ic.waitForShutdown();
        } catch (Ice.LocalException e) {
            e.printStackTrace();
            status = 1;
        } catch (Exception e) {
            System.err.println(e.getMessage());
            status = 1;
        }
        if (ic != null) {
            // Clean up
            try {
                ic.destroy();
            } catch (Exception e) {
                System.err.println(e.getMessage());
                status = 1;
            }
        }
        System.exit(status);
    }
}