package eu.dl.worker.master;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;

/**
 * Compares tenders based on maximum publication
 * date("publications.publicationDate").
 * 
 * @param <T>
 *            tender type to be compared
 */
public class TenderDateComparator<T extends MatchedTender> implements Comparator<T> {

    @Override
    public final int compare(final T o1, final T o2) {
        LocalDate o1Max = getMaxDate(o1.getPublications());
        LocalDate o2Max = getMaxDate(o2.getPublications());

        if (o1Max == null && o2Max == null) {
            return 0;
        }

        if (o1Max == null && o2Max != null) {
            return -1;
        }

        if (o1Max != null && o2Max == null) {
            return 1;
        }
        
        return o1Max.compareTo(o2Max);
    }

    /**
     * Calculates the max publication date from the publications collection.
     * 
     * @param publications
     *            collection of publications
     * @return max date found, null if no date filled at all
     */
    private LocalDate getMaxDate(final List<Publication> publications) {
        LocalDate max = null;
        for (Publication publication : publications) {
            if (publication.getPublicationDate() != null) {
                if (max == null) {
                    max = publication.getPublicationDate();
                } else if (publication.getPublicationDate().isAfter(max)) {
                    max = publication.getPublicationDate();
                }
            }
        }
        return max;
    }
}
