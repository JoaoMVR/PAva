package ist.meic.pa;

import java.util.*;

public final class Utils {

    private Utils() { }

    public static <K, V> void putAllIfAbsent(final Map<K, V> target, 
                                             final Map<K, V> source) {
        for (Map.Entry<K, V> entry : source.entrySet()) {
            target.putIfAbsent(entry.getKey(), entry.getValue());
        }
    }

} // class Utils
