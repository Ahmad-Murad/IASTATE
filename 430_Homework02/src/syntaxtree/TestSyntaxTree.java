package syntaxtree;

import org.junit.Test;

public class TestSyntaxTree
{
    // Test 1: can several threads concurrently parse different files?
    // All three threads should start parsing at about the same time.
    @Test
    public void test1()
    {
        // original time -- 15s
        System.out.println("\nstarting test 1:");
        SyntaxTreeManager m = new SyntaxTreeManager();
        Thread thread1 = new TestThread("Test thread 1", "file1", m);
        Thread thread2 = new TestThread("Test thread 2", "file2", m);
        Thread thread3 = new TestThread("Test thread 3", "file3", m);
        thread1.start();
        thread2.start();
        thread3.start();

        try
        {
            thread1.join();
            thread2.join();
            thread3.join();
        } catch (InterruptedException e)
        {
        }
    }

    // Test 2: if another thread tries to get the syntax tree for
    // the same file, does it get parsed multiple times?  Only one
    // of the threads should parse the file, the others should block
    // until the file is parsed.
    @Test
    public void test2()
    {
        // orig time -- 5s
        System.out.println("\nstarting test 2:");
        SyntaxTreeManager m = new SyntaxTreeManager();
        Thread thread1 = new TestThread("Test thread 1", "file1", m);
        Thread thread2 = new TestThread("Test thread 2", "file1", m);
        Thread thread3 = new TestThread("Test thread 3", "file1", m);
        thread1.start();
        thread2.start();
        thread3.start();

        try
        {
            thread1.join();
            thread2.join();
            thread3.join();
        } catch (InterruptedException e)
        {
        }
    }

    // Test 3: if a thread is blocked trying to get a syntax tree,
    // can it be interrupted?  We should see thread 2 throw an
    // InterruptedException
    @Test
    public void test3() throws InterruptedException
    {
        // orig time -- 1s
        System.out.println("\ntest 3:");
        SyntaxTreeManager m = new SyntaxTreeManager();
        Thread thread1 = new TestThread("Test thread 1", "file1", m);
        thread1.start();

        // wait a half second
        try
        {
            Thread.sleep(500);
        } catch (InterruptedException e)
        {
        }

        // thread 1 should have started parsing, so thread 2 should
        // block trying to get the syntax tree for file1
        Thread thread2 = new TestThread("Test thread 2", "file1", m);
        thread2.start();

        // wait a half second
        try
        {
            Thread.sleep(500);
        } catch (InterruptedException e)
        {
        }

        // now interrupt thread 2
        thread2.interrupt();

        thread1.join();
        thread2.join();
    }

    private static class TestThread extends Thread
    {
        private String m_filename;
        private SyntaxTreeManager m_manager;

        public TestThread(String threadName, String filename, SyntaxTreeManager manager)
        {
            super(threadName);
            m_filename = filename;
            m_manager = manager;
        }

        @Override
        public void run()
        {
            try {
                SyntaxTree ast = m_manager.getSyntaxTree(m_filename);
                System.out.println(Thread.currentThread().getName() + " got syntax tree for: " + ast.getName());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}
