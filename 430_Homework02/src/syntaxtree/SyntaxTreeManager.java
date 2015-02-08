package syntaxtree;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * The modifications I made to this class were primarily making the SyntaxTreeHolder
 * class implement Future<SyntaxTree>. In the getSyntaxTree() method, I reduced the
 * amount of time spent blocking by not parsing the file inside of the synchronized
 * part of this method. If a filename was NOT cached on the tree yet, the method
 * drops into the sync block, checks if it's still null, and if it is then the method
 * builds a new future for the SyntaxTree and sticks the future into the m_cache and
 * then releases its sync. After the sync is release, the thread attempts to get
 * the future, which will block (but this is OK because it's not blocking other gets).
 *
 * @author Andrew
 */
public class SyntaxTreeManager
{

    /**
     * The actual cache of syntax trees
     */
    private final Map<String, SyntaxTreeHolder> m_cache = new HashMap<>();
    private ExecutorService exec = Executors.newCachedThreadPool();

    /**
     * Return the syntax tree for the given source file, parsing
     * the file if necessary.
     *
     * @param filename
     * @return
     */
    public SyntaxTree getSyntaxTree(String filename) throws InterruptedException
    {
        SyntaxTreeHolder holder = m_cache.get(filename);
        if (holder == null)
            synchronized (this) {
                holder = m_cache.get(filename);
                // If a STH has not been created yet for this file,
                // create one and put it into the cache
                if (holder == null) {
                    holder = new SyntaxTreeHolder(filename);
                    m_cache.put(filename, holder);
                }
            }

        // Get the future once the sync has been released, the thread will block
        // here which is what we want because it's not blocking other gets.
        return holder.get();
    }

    private class SyntaxTreeHolder implements Future<SyntaxTree>
    {
        private Future<SyntaxTree> m_ast;

        public SyntaxTreeHolder(final String fileName) {
            // Submit a new task to parse the file, it's important that we
            // don't block here because this constructor needs to finish quickly!
            System.out.println("** Submitting a new task for file " + fileName);
            m_ast = exec.submit(new Callable<SyntaxTree>() {
                @Override
                public SyntaxTree call() throws Exception {
                    return new Parser(fileName).parse();
                }
            });
        }

        @Override
        public SyntaxTree get() throws InterruptedException {
            try {
                return m_ast.get();
            } catch (ExecutionException e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public boolean cancel(boolean arg0) {
            return m_ast.cancel(arg0);
        }

        @Override
        public SyntaxTree get(long arg0, TimeUnit arg1) throws InterruptedException, ExecutionException, TimeoutException {
            return m_ast.get(arg0, arg1);
        }

        @Override
        public boolean isCancelled() {
            return m_ast.isCancelled();
        }

        @Override
        public boolean isDone() {
            return m_ast.isDone();
        }
    }
}