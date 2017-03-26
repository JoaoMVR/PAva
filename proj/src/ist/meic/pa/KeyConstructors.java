package ist.meic.pa;

import javassist.Loader;
import javassist.ClassPool;

public class KeyConstructors {
  
    public static void main(String[] args) throws Throwable {
        final int minArgCount = 1;

        if (args.length < minArgCount) {
            System.err.println("Usage: java KeyConstructors <class>");
            System.exit(1);
        }

        final String[] restArgs = new String[args.length - minArgCount];
        System.arraycopy(args, minArgCount, restArgs, 0, restArgs.length);

        final Loader classLoader = new Loader();
        classLoader.addTranslator(ClassPool.getDefault(), new KeywordTranslator());
        classLoader.run(args[0], restArgs);
    }

}
