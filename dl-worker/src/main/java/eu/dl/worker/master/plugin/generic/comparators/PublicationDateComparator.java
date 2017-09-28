package eu.dl.worker.master.plugin.generic.comparators;

import eu.dl.dataaccess.dto.matched.MasterablePart;

import java.time.LocalDate;
import java.util.Comparator;

/**
 * This comparator helps to order tender parts in the publication date order.
 *
 * @param <T> part of the tender
 */
public class PublicationDateComparator<T extends MasterablePart> implements Comparator<T> {
    @Override
    public final int compare(final T o1, final T o2) {
        LocalDate o1Date = o1.getPublicationDate();
        LocalDate o2Date = o2.getPublicationDate();

        if (o1Date == null && o2Date == null) {
            return 0;
        }

        if (o1Date == null) {
            return -1;
        }

        if (o2Date == null) {
            return 1;
        }

        return o1Date.compareTo(o2Date);
    }
}