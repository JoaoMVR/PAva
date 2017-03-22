package ist.meic.pa;

import javassist.*;

public class KeyConstructors {

    public static void main(String[] args) {
        final int minArgCount = 1;

        if (args.length < minArgCount) {
            System.err.println("Usage: java KeyConstructors <class>");
            System.exit(1);
        }

        final Translator translator = new KeywordTranslator();
        final ClassPool pool        = ClassPool.getDefault();
        final Loader classLoader    = new Loader();

        try {
            classLoader.addTranslator(pool, translator);
        } catch (NotFoundException | CannotCompileException e) {
            // FIXME:TODO Auto-generated catch block
            e.printStackTrace();
        }

        final String[] restArgs = new String[args.length - minArgCount];
        System.arraycopy(args, minArgCount, restArgs, 0, restArgs.length);

        try {
            classLoader.run(args[0], restArgs);
        } catch (Throwable e) {
            // FIXME:TODO Auto-generated catch block
            e.printStackTrace();
        } //runs the Test provided as input

    }

}
