package ist.meic.pa;

import javassist.*;

public class KeyConstructors {
	
	/*
	 * TODO: Ask professor about throwing throwable
	 */

    public static void main(String[] args) throws Throwable {
        final int minArgCount = 1;

        if (args.length < minArgCount) {
            System.err.println("Usage: java KeyConstructors <class>");
            System.exit(1);
        }
/*
<<<<<<< HEAD
        final Translator translator = new KeywordTranslator();
        final ClassPool pool        = ClassPool.getDefault();
        final Loader classLoader    = new Loader();

        classLoader.addTranslator(pool, translator);
      

        final String[] restArgs = new String[args.length - minArgCount];
        System.arraycopy(args, minArgCount, restArgs, 0, restArgs.length);

=======*/
        final String[] restArgs = new String[args.length - minArgCount];
        System.arraycopy(args, minArgCount, restArgs, 0, restArgs.length);

        final Loader classLoader = new Loader();
        classLoader.addTranslator(ClassPool.getDefault(), new KeywordTranslator());
// >>>>>>> process-annotations
        classLoader.run(args[0], restArgs);
    }

}
