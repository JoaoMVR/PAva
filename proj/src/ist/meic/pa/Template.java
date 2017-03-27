package ist.meic.pa;

import javassist.CtClass;
import javassist.NotFoundException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

/**
 * Class that builds the code intended to be injected by KeywordTranslator.
 */
public final class Template {

    private Template() { }

    /**
     * The only public method in this class. This is the called to obtain the
     * code intended to be injected by KeywordTranslator.
     */
    public static String build(CtClass c) throws ClassNotFoundException, NotFoundException {
        return makeTemplate(getAllDefaultAssignments(c));
    }

    /**
     * @param  defaultAssignments a map of the default assignments for the
     *         KeywordArgs annotated constructor.
     * @result code intended to be inserted in the annotated constructor's body,
     *         that handles the fields assignments.
     *
     * Pros of this approach:
     *   1. Super simple implementation.
     *   2. We can use Javssist and Java 8.
     *
     * Cons of this approach:
     *   1. Possibly two assignments (one for the default and possibly another
     *      one) to each field, which may be a problem if the arguments are
     *      computationally intensive.
     *   2. Reflection is slower than hardcoded comparisons.
     */
    private static String makeTemplate(Map<String,String> defaultAssignments) {
        String template = "{";

        // Args size must be even, obviously.
        template += "TemplateCode.assertArgsHaveEvenLength($1);";

        for (Map.Entry<String,String> da : defaultAssignments.entrySet()) {
            template += "$0." + da.getKey() + "=" + da.getValue() + ";";
        }

        template += "for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {"
                 +  "    final String kword = (String) $1[i];"
                 +  "    final Object value = $1[j];"
                 +  "    TemplateCode.setField(kword, value, $0);"
                 +  "}";

        return template + "}";
    }

    /**
     * @param  clazz with the KeywordsArg annotated constructor.
     * @return map of default assignments for all KeywordArgs annotated
     *         constructors in the clazz hierarchy. A map like [("height",
     *         "10"), ("width", "5"), ("margin", "1"), ("name", "Extended")].
     */
    private static Map<String,String> getAllDefaultAssignments(final CtClass clazz) throws NotFoundException, ClassNotFoundException {
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
    private static Map<String,String> getDefaultAssignments(final CtClass clazz) throws ClassNotFoundException {
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
    private static Map<String,String> getDefaultAssignments(final KeywordArgs annotation) {
        final Map<String,String> assignments = new HashMap<>();

        Utils.split(annotation.value())
            .stream()
            .filter(s -> s.contains("=")) // i.e. if it has a default value.
            .map(s -> s.split("=", 2))   // sp[0] will be the keyword and sp[1] the value.
            .forEach(sp -> assignments.put(sp[0], sp[1]));

        return assignments;
    }

} // class Template
