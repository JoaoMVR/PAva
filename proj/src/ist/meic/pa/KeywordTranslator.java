package ist.meic.pa;

import javassist.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;

public class KeywordTranslator implements Translator {

    // FIXME: If it is empty why do we override this?
    @Override
    public void start(ClassPool arg0) throws NotFoundException,
                                             CannotCompileException {
        // Empty.
    }

    /**
     *  This is the method that will modify each class in the classpool.
     */
    // FIXME: Missing documentation.
    @Override
    public void onLoad(ClassPool pool, String className)
        throws NotFoundException, CannotCompileException {
        try {
            makeConstructor(pool.get(className));
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    //==========================================================================
    // Private methods
    //==========================================================================

    // FIXME: This should be decoupled from the translator so that we can debug
    // the generated code by hand.
    // FIXME: Missing documentation.
    private void makeConstructor(CtClass ctClass)
        throws ClassNotFoundException, CannotCompileException, NotFoundException {

        for(CtConstructor ctConstructor: ctClass.getConstructors()) {
            if (ctConstructor.getAnnotation(KeywordArgs.class) instanceof KeywordArgs) {
                final KeywordArgs ann =
                    (KeywordArgs) ctConstructor.getAnnotation(KeywordArgs.class);

                //adds new constructor for inheritance instantiations
                ctClass.addConstructor(CtNewConstructor.defaultConstructor(ctClass));

                treatAnnotations(ctConstructor, ann, ctClass);
            }
        }
    }

    // FIXME: This should be decoupled from the translator so that we can debug
    // the generated code by hand.

    /**
     * FIXME: Missing documentation.
     * IN
     *     @KeywordArgs("width=100,height=50,margin")
     *     public Widget(Object... args) {}

     * OUT
     *     public Widget(Object... args) {
     *       this.width = 100;
     *       this.height = 50;

     *       initialize set with class fields (probably not needed);
     *       assert args.length is even;

     *       for each (kword, value) in args:
     *         if field with name kword exists:
     *           this.'kword = value;
     *         else:
     *           throw new RuntimeException("unsuported kword");
     *     }
     * Instantiate like: new Widget("height", 10, "width", 2)
     * @throws NotFoundException
     * @throws ClassNotFoundException
     */
    private void treatAnnotations(CtConstructor ctConstructor, KeywordArgs annotation, CtClass ctClass)
        throws CannotCompileException, NotFoundException, ClassNotFoundException {
        ctConstructor.setBody(makeTemplate(defaultAssignments(annotation, ctClass)));
    }

    // FIXME: Missing documentation.
    // FIXME: Problems with this approach:
    //          1. Possibly two assignments to each field which may be a problem
    //             if the arguments (both to the annotation and to the constructor)
    //             are computationally intensive.
    //          2. Using reflection is significantly slower than hardcoded comparisons.
    //          3. This doesn't handle keywords that reference each other.
    //          4. Doesn't handle empty keywords.
    //          5. Doesn't handle inheritance.
    //			6. Doesn't verify args size, which has to be even
    private String makeTemplate(List<String> defaultAssignments) {
        String template = "{\n";

        for (String da : defaultAssignments) {
            // If da.equals("height=10") we get "this.height=10;".
            template += "    $0." + da + ";" + "\n";
        }

        template +=                                                                           "\n"
            + "    for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {" +                 "\n"
            + "        final String kword = (String) $1[i];" +                                "\n"
            + "        final Object value = $1[j];" +                                         "\n"
            +                                                                                 "\n"
            + "        try {" +                                                               "\n"
            + "            final java.lang.reflect.Field field = " +                          "\n"
            + "                $0.getClass().getDeclaredField(kword);" +                      "\n"
            + "            field.setAccessible(true);" +                                      "\n"
            + "            field.set($0, value);" +                                           "\n"
            + "        } catch (NoSuchFieldException e) {" +                                  "\n"
            + "	            java.lang.Class superClass = $0.getClass().getSuperclass();"+	  "\n"
            + "				while(superClass.getName() != \"java.lang.Object\") {" +		  "\n"
            + "					try{" +														  "\n"
            + "            	   		final java.lang.reflect.Field field = " +                 "\n"
            + "                			superClass.getDeclaredField(kword);" + 				  "\n"
            + "           			field.setAccessible(true);" +     	             	      "\n"
            + "         			field.set($0, value);" +            	                  "\n"
            + "						break;" +												  "\n"
            + "           		} catch(NoSuchFieldException e) {" +						  "\n"
            + "						superClass = superClass.getSuperclass();" +	  "\n"
            + "						if(superClass.getName() != \"java.lang.Object\") continue;"+"\n"
            + "						else {" +												  "\n"
            + "		   					throw new RuntimeException(\"Unrecognized keyword: \" + kword);" +"\n"
            + "						}" +													  "\n"
            + "			  		}" +														  "\n"
            + "				}" +															  "\n"
            + "        } catch (Exception e) {" +                                             "\n"
            + "            throw new RuntimeException(e);" +                                  "\n"
            + "        }" +                                                                   "\n"
            + "    }" +                                                                       "\n";

        template += "}";
        return template;
    }

    /**
     * FIXME: Missing documentation.
     * defaultAssignments("width=100,height=50,margin") =
     *    ["width=100","height=50"]
     * @throws NotFoundException
     * @throws ClassNotFoundException
     */
    private List<String> defaultAssignments(KeywordArgs annotation, CtClass ctClass)
        throws NotFoundException, ClassNotFoundException {
        // FIXME:TODO
        List<String> keywords = processAnnotation(annotation);
        parentAssignments(ctClass, keywords);
        return keywords;
    }

    private void parentAssignments(CtClass ctClass, List<String> keywords)
        throws NotFoundException, ClassNotFoundException {

        // FIXME: What about interfaces?
        if(ctClass.getName().equals("java.lang.Object")) {
            return;
        } else {
            ctClass = ctClass.getSuperclass();

            for(CtConstructor ctConstructor : ctClass.getConstructors()) {
                if(ctConstructor.hasAnnotation(KeywordArgs.class)) {
                    final KeywordArgs annotation =
                        (KeywordArgs) ctConstructor.getAnnotation(KeywordArgs.class);
                    List<String> superKeywords = processAnnotation(annotation);

                    checkDuplicates(keywords, superKeywords);
                }
            }

            parentAssignments(ctClass, keywords);
        }
    }

    /**
     * Checks whether the start of a keyword has already been seen. For
     * instance: If you have "height=10" in the subclass and "height=20" in
     * the superclass, the last one should be ignored.
     *
     * So, if the start of a keyword in a super class hasn't been seen before in
     * the list of keywords, then it can be added.
     */
    private void checkDuplicates(List<String> keywords, List<String> superKeywords) {
        boolean controlVariable;

        for (String superKeyword : superKeywords) {
            controlVariable = true;

            for (String keyword : keywords) {
                String[] splitString = superKeyword.split("=");

                if (splitString.length != 1) // Case of single parameter without "=", "aka", "margin"
                    if (keyword.startsWith(splitString[0])) {
                        controlVariable = false;
                        break;
                    }
            }

            if(controlVariable) {
                keywords.add(superKeyword);
            }
        }
    }


    /**
     * Gets the default keyword assignments given in the annotation.
     *
     * For instance, if the annotation is the following
     *
     *   @KeywordArgs("width=100,height=50,margin=5")
     *
     * then the resulting list should be
     *
     *   {"width=100","height=50"}.
     *
     * @param  annotation to be processed.
     * @return list of keyword assignments of the form "keyword=value".
     *
     * TODO: Check if this works in cases where ',' or '=' appear inside a
     * string literal.
     *
     */
    private List<String> processAnnotation(final KeywordArgs annotation) {
        final List<String> keywords    = new ArrayList<>();
        final String[] splitAnnotation = annotation.value().split(",");

        for (final String split : splitAnnotation) {
            if (split.contains("=")) { // Then there exists a default value.
                keywords.add(split);
            }
        }

        return keywords;
    }
}
