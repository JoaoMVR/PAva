package ist.meic.pa;

import javassist.*;

import java.util.*;
import java.util.stream.*;

public final class Template {

    private Template() { }

    public static String build(CtClass c) throws ClassNotFoundException, NotFoundException {
        return makeTemplate(getAllDefaultAssignments(c));
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
    public static String makeTemplate(Map<String,String> defaultAssignments) {
        String template = "{\n";

        for (Map.Entry<String,String> da : defaultAssignments.entrySet()) {
            template += "    $0." + da.getKey() + "=" + da.getValue() + ";" + "\n";
        }

        template +=                                                                                        "\n"
            + "    for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {"                              + "\n"
            + "        final String kword = (String) $1[i];"                                             + "\n"
            + "        final Object value = $1[j];"                                                      + "\n"
            + ""                                                                                         + "\n"
            + "        try {"                                                                            + "\n"
            + "            final java.lang.reflect.Field field = "                                       + "\n"
            + "                $0.getClass().getDeclaredField(kword);"                                   + "\n"
            + ""                                                                                         + "\n"
            + "            field.setAccessible(true);"                                                   + "\n"
            + "            field.set($0, value);"                                                        + "\n"
            + "        } catch (NoSuchFieldException e) {"                                               + "\n"
            + "              java.lang.Class superClass = $0.getClass().getSuperclass();"                + "\n"
            + ""                                                                                         + "\n"
            + "              while(!superClass.getName().equals(\"java.lang.Object\")) {"                      + "\n"
            + "                  try {"                                                                  + "\n"
            + "                      final java.lang.reflect.Field field = "                             + "\n"
            + "                          superClass.getDeclaredField(kword);"                            + "\n"
            + ""                                                                                         + "\n"
            + "                      field.setAccessible(true);"                                         + "\n"
            + "                      field.set($0, value);"                                              + "\n"
            + ""                                                                                         + "\n"
            + "                      break;"                                                             + "\n"
            + "                  } catch(NoSuchFieldException e) {"                                      + "\n"
            + "                      superClass = superClass.getSuperclass();"                           + "\n"
            + ""                                                                                         + "\n"
            + "                      if (superClass.getName().equals(\"java.lang.Object\")) {"                      + "\n"
            + "                         throw new RuntimeException(\"Unrecognized keyword: \" + kword);" + "\n"
            + "                      }"                                                                  + "\n"
            + "                  }"                                                                      + "\n"
            + "              }"                                                                          + "\n"
            + "        } catch (Exception e) {"                                                          + "\n"
            + "            throw new RuntimeException(e);"                                               + "\n"
            + "        }"                                                                                + "\n"
            + "    }"                                                                                    + "\n";

        template += "}";
        return template;
    }

    /**
     * @param  clazz the class which ctor belongs to.
     * @return map of default assignments. A map like
     *         [("height", "10"), ("width", "5"), ("margin", "1")].
     */
    public static Map<String,String> getAllDefaultAssignments(final CtClass clazz) throws NotFoundException, ClassNotFoundException {
        final Map<String,String> assignments = new HashMap<>();

        for (CtClass c = clazz; !c.getName().equals("java.lang.Object"); c = c.getSuperclass()) {
            Utils.putAllIfAbsent(assignments, getDefaultAssignments(c));
        }

        return assignments;
    }

    public static Map<String,String> getDefaultAssignments(final CtClass clazz) throws ClassNotFoundException {
        try {
            final KeywordArgs ann = (KeywordArgs) Stream.of(clazz.getConstructors())
                .filter(ctor -> ctor.hasAnnotation(KeywordArgs.class))
                .findFirst() // There will be zero or one, not more.
                .get()
                .getAnnotation(KeywordArgs.class);

            return preprocessAnnotation(ann);
        } catch (NoSuchElementException e) {
            return new HashMap<>();
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
     * XXX: Update docs.
     *
     */
    public static Map<String,String> preprocessAnnotation(final KeywordArgs annotation) {
        final Map<String,String> assignments = new HashMap<>();

        // FIXME: What about if the default value is a string containing "," ?
        Stream.of(annotation.value().split(","))
            .filter(s -> s.contains("="))
            .map(s -> s.split("=", 2))
            .forEach(sp -> assignments.put(sp[0], sp[1]));

        return assignments;
    }

} // class Template
