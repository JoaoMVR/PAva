package ist.meic.pa;

import javassist.CtClass;
import javassist.NotFoundException;
import javassist.bytecode.Descriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Utilities class, used to store functions that don't seem to belong anywhere
 * else.
 */
public final class Utils {

    private Utils() { }

    public static <K, V> void putAllIfAbsent(final Map<K, V> target,
                                             final Map<K, V> source) {
        for (Map.Entry<K, V> entry : source.entrySet()) {
            target.putIfAbsent(entry.getKey(), entry.getValue());
        }
    }

    public static boolean hasDefaultConstructor(CtClass c) {
        try {
            c.getConstructor(Descriptor.ofConstructor(new CtClass[0]));
        } catch (NotFoundException e) {
            return false;
        }
        return true;
    }

    /**
     * Splits a string on commas, ignoring the ones inside quotes.
     *
     * Credit to http://stackoverflow.com/questions/1757065/java-splitting-a-comma-separated-string-but-ignoring-commas-in-quotes/2120714#2120714
     */
    public static List<String> split(String s) {
        final List<String> result = new ArrayList<String>();

        int start        = 0;
        boolean inQuotes = false;

        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\"')  {
                inQuotes = !inQuotes;
            }

            if (i == s.length() - 1) {
                result.add(s.substring(start));
            } else if (s.charAt(i) == ',' && !inQuotes) {
                result.add(s.substring(start, i));
                start = i + 1;
            }
        }

        return result;
    }

    /**
     * @param  clazz
     * @return a list including clazz and all its superclasses up to, but
     *         excluding, Object (in this order).
     */
    public static List<Class> getHierarchy(final Class clazz) {
        final List<Class> hierarchy = new ArrayList<>();

        for (Class c = clazz; c != Object.class; c = c.getSuperclass()) {
            hierarchy.add(c);
        }

        return hierarchy;
    }

} // class Utils
