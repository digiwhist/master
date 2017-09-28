package eu.dl.worker.master.plugin;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Document Mastering plugin. Matches documents of matched tender lots and creates master record for each document.
 *
 * @param <T>
 *         implementation type (class)
 * @param <W>
 *         implementation type (class)
 * @param <U>
 *         implementation type (class) for Document
 * @param <X>
 *         implementation type (class) for Document
 */
public abstract class BaseDocumentPlugin<T extends MasterablePart, W, U extends Document, X extends Document>
        extends BaseMatchAndMasterPlugin<T, W, U, X> {

    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "documentPlugin";

    @Override
    protected final List<List<U>> getListsForMatching(final List<T> items) {
        if (items.get(0) instanceof MatchedTender) {
            return items.stream().map(t -> (List<U>) ((MatchedTender) t).getDocuments()).filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } else if (items.get(0) instanceof MatchedBid) {
            return items.stream().map(t -> (List<U>) ((MatchedBid) t).getDocuments()).filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }

        logger.error("Can't get documents, unknown ancestor");
        throw new UnrecoverableException("Can't get documents, unknown ancestor");
    }

    @Override
    protected final List<List<U>> match(final List<List<U>> inputListsForMatching) {
        if (inputListsForMatching.isEmpty()) {
            return null;
        }

        List<U> allDocuments = inputListsForMatching
                .stream()
                .filter(item -> item != null)
                .flatMap(List::stream)
                .collect(Collectors.toList());

        // matched documents by source and isEuFund
        final HashMap<String, List<U>> matchedDocuments = new HashMap<>();

        for (U document : allDocuments) {
            String hash = UUID.randomUUID().toString();

            if (document != null && document.getUrl() != null) {
                hash = document.getUrl().toString();
            }
            if (matchedDocuments.get(hash) == null) {
                matchedDocuments.put(hash, new ArrayList<>(Collections.singletonList(document)));
            } else {
                matchedDocuments.get(hash).add(document);
            }
        }


        List<List<U>> result = new ArrayList<>();
        result.addAll(matchedDocuments.values());

        return result;
    }

    @Override
    protected final W setFinalList(final W finalItem, final List<X> finalList) {
        if (finalItem instanceof MasterTender) {
            return (W) ((MasterTender) finalItem).setDocuments((List<Document>) finalList);
        } else if (finalItem instanceof MasterBid) {
            return (W) ((MasterBid) finalItem).setDocuments((List<Document>) finalList);
        }

        logger.error("Can't get documents, unknown ancestor");
        throw new UnrecoverableException("Can't get documents, unknown ancestor");
    }

    @Override
    protected final X setSourceStructuredIds(final List<U> matchedList, final X masterItem) {
        return masterItem;
    }
}
