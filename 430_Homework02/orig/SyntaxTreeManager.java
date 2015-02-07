package syntaxtree;

import java.util.HashMap;
import java.util.Map;

/**
 * Singleton class for managing the abstract syntax trees obtained
 * from parsing source files.
 *
 * In this version, a sync lock is held
 * on the SyntaxTreeManager for the duration of every
 * call to the parser.
 */
public class SyntaxTreeManager
{

    /**
     * The actual cache of syntax trees
     */
    private final Map<String, SyntaxTreeHolder> m_cache = new HashMap<String, SyntaxTreeHolder>();

    /**
     * Return the syntax tree for the given source file, parsing
     * the file if necessary.
     * 
     * @param filename
     * @return
     */
    public synchronized SyntaxTree getSyntaxTree(String filename)
    {
        SyntaxTreeHolder holder = m_cache.get(filename);
        if (holder == null)
        {
            Parser parser = new Parser(filename);
            SyntaxTree ast = parser.parse();
            holder = new SyntaxTreeHolder();
            holder.set(ast);
            m_cache.put(filename, holder);
        }
        return holder.get();
    }

    /**
     * Helper class for storing completed syntax trees. Doesn't
     * do much at this point, but could be useful...
     */
    private static class SyntaxTreeHolder
    {
        private SyntaxTree m_ast;

        public SyntaxTree get()
        {
            return m_ast;
        }

        public void set(SyntaxTree ast)
        {
            m_ast = ast;
        }
    }
}