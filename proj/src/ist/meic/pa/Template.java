package ist.meic.pa;

import javassist.CtClass;
import javassist.NotFoundException;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

public final class Template {

    private Template() { }

    public static String build(CtClass c) throws ClassNotFoundException, NotFoundException {
        return makeTemplate(getAllDefaultAssignments(c));
    }

    /**
     * @param  defaultAssignments a map of the default assignments for the
     *         KeywordArgs annotated constructor.
     * @result code intended to be inserted in the annotated constructor's body,
     *         that handles the fields assignments.
     *
     * FIXME: Problems with this approach:
     *   1. Possibly two assignments (one for the default and possibly another
     *      one) to each field, which may be a problem if the arguments are
     *      computationally intensive.
     *   2. Using reflection is significantly slower than hardcoded comparisons.
     *   3. Doesn't handle keywords that reference each other.
     */
    public static String makeTemplate(Map<String,String> defaultAssignments) {
        String template = "{\n";

        // Args size must be even, obviously.
        template +=                                                       "\n"
            + "if ($1.length % 2 != 0)"                                 + "\n"
            + "   throw new RuntimeException(\"Odd argument count.\");" + "\n";

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
            + "              while(!superClass.getName().equals(\"java.lang.Object\")) {"                + "\n"
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
            + "                      if (superClass.getName().equals(\"java.lang.Object\")) {"           + "\n"
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
     * @param  clazz with the KeywordsArg annotated constructor.
     * @return map of default assignments for all KeywordArgs annotated
     *         constructors in the clazz hierarchy. A map like [("height",
     *         "10"), ("width", "5"), ("margin", "1"), ("name", "Extended")].
     */
    public static Map<String,String> getAllDefaultAssignments(final CtClass clazz) throws NotFoundException, ClassNotFoundException {
        final Map<String,String> assignments = new HashMap<>();

        for (CtClass c = clazz; !c.getName().equals("java.lang.Object"); c = c.getSuperclass()) {
            Utils.putAllIfAbsent(assignments, getDefaultAssignments(c));
        }

        return assignments;
    }

    /**
     * @param  clazz with the KeywordsArg annotated constructor.
     * @return map of default assignments for the KeywordArgs annotated
     *         constructor in this clazz. A map like [("height", "10"),
     *         ("width", "5"), ("margin", "1")].
     */
    public static Map<String,String> getDefaultAssignments(final CtClass clazz) throws ClassNotFoundException {
        try {
            final KeywordArgs ann = (KeywordArgs) Stream.of(clazz.getConstructors())
                .filter(ctor -> ctor.hasAnnotation(KeywordArgs.class))
                .findFirst() // There should be zero or one, not more.
                .get()
                .getAnnotation(KeywordArgs.class);

            return getDefaultAssignments(ann);
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
     * then the resulting map should be
     *
     *   [("height", "50"), ("width", "100"), ("margin", "5")]
     *
     * @param  annotation to be processed.
     * @return map of keyword assignments of the form (keyword, value).
     *
     */
    public static Map<String,String> getDefaultAssignments(final KeywordArgs annotation) {
        final Map<String,String> assignments = new HashMap<>();

        Utils.split(annotation.value())
            .stream()
            .filter(s -> s.contains("="))
            .map(s -> s.split("=", 2))
            .forEach(sp -> assignments.put(sp[0], sp[1]));

        return assignments;
    }

} // class Template
