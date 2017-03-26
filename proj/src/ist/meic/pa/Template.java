package ist.meic.pa;

import javassist.CtClass;
import javassist.NotFoundException;

import java.lang.reflect.Field;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
     */
    public static String makeTemplate(Map<String,String> defaultAssignments) {
        String template = "{";

        // Args size must be even, obviously.
        template += "Template.assertArgsHaveEvenLength($1);";

        for (Map.Entry<String,String> da : defaultAssignments.entrySet()) {
            template += "$0." + da.getKey() + "=" + da.getValue() + ";";
        }

        template += "for (int i = 0, j = 1; j < $1.length; i += 2, j += 2) {"
                 +  "    final String kword = (String) $1[i];"
                 +  "    final Object value = $1[j];"
                 +  "    Template.setField(kword, value, $0);"
                 +  "}";

        return template + "}";
    }

    /**
     * Asserts its arguments have even length.
     */
    public static void assertArgsHaveEvenLength(Object[] args) {
        if (args.length % 2 != 0) {
            throw new RuntimeException("Odd argument count.");
        }
    }

    /**
     * Searches for the first field of name fieldName on the hierarchy of
     * classes of obj (wether it is private, public, protected or
     * package-private) and sets its field to value.
     *
     * @param fieldName the name of the field to search for.
     * @param value the value to set the field to.
     * @param obj the obj for which we want to set the field.
     */
    public static void setField(String fieldName, Object value, Object obj) {
        try {
            final Field field = Utils.getHierarchy(obj.getClass())
                .stream()
                .flatMap(c -> Stream.of(c.getDeclaredFields()))
                .filter(f -> f.getName().equals(fieldName))
                .findFirst()
                .get();

            field.setAccessible(true);
            field.set(obj, value);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (NoSuchElementException e) {
            throw new RuntimeException("Unrecognized keyword: " + fieldName);
        }
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
            .filter(s -> s.contains("=")) // i.e. if it has a default value.
            .map(s -> s.split("=", 2))   // sp[0] will be the keyword and sp[1] the value.
            .forEach(sp -> assignments.put(sp[0], sp[1]));

        return assignments;
    }

} // class Template
