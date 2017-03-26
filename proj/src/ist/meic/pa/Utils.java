package ist.meic.pa;

import javassist.CtClass;
import javassist.NotFoundException;
import javassist.bytecode.Descriptor;

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

} // class Utils
