package eu.datlab.worker.fr.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Web tender handler for France.
 *
 * @author Marek Mikes
 */
final class BOAMPWebTenderHandler {
    /**
     * This invisible character appears around publication source ID.
     */
    private static final Character CURIOUS_WHITE_SPACE = (char) 160;

    /**
     * Private constructor to make this class static.
     */
    private BOAMPWebTenderHandler() {
    }

    /**
     * Parses document from the web, where we want only publications information.
     *
     * @param document         document to be parsed
     * @param humanReadableUrl HTML page URL
     * @param parsedTender     tender to add data to
     */
    static void parse(final Document document, final String humanReadableUrl, final ParsedTender parsedTender) {
        // get the publication
        // we parse publication date even if the date is in XML because of format ("07/05/2010" vs "07 MAI       2010")
        Element publicationDateElement = JsoupUtils.selectFirst("article > header > p.date-publishing", document);
        // sometimes the page does not exists (see http://www.boamp.fr/avis/detail/BXP052350506446) and the date is null
        String publicationDate = publicationDateElement == null
                ? null
                : publicationDateElement.ownText().replace("publié le", "").trim();
        addPublicationTo(parsedTender, true, humanReadableUrl.substring(humanReadableUrl.lastIndexOf('/') + 1),
                PublicationSources.FR_BOAMP_WEB, humanReadableUrl, publicationDate);

        // get related publications
        final Elements relatedPublications = JsoupUtils.select("aside.avis-related > ul > li", document);
        for (Element relatedPublication : relatedPublications) {
            Element relatedPublicationLink = JsoupUtils.selectFirst("h3 > a", relatedPublication);
            final String relatedPublicationUrl = PublicationSources.FR_BOAMP_WEB +
                    JsoupUtils.selectAttribute("href", relatedPublicationLink);
            String linkText = relatedPublicationLink.ownText().replace("Annonce n°" + CURIOUS_WHITE_SPACE, "").trim();
            if (linkText.length() >= 2) {
                assert linkText.charAt(linkText.length() - 2) == CURIOUS_WHITE_SPACE
                        && linkText.charAt(linkText.length() - 1) == ':';
                linkText = linkText.substring(0, linkText.length() - 2);
            }
            final String sourceId = linkText;
            publicationDateElement = JsoupUtils.selectFirst("p.date-publishing", relatedPublication);
            publicationDate = publicationDateElement.ownText().replace("Parue le", "").trim();
            addPublicationTo(parsedTender, false, sourceId, PublicationSources.FR_BOAMP_WEB, relatedPublicationUrl,
                    publicationDate);
        }

        // get TED or initial notice
        // - TED publication is referenced here: http://www.boamp.fr/avis/detail/15-32095
        // - initial notice is referenced here:
        //   - http://www.boamp.fr/avis/detail/10-10115
        //   - http://www.boamp.fr/avis/detail/BWP07299032184I
        final Elements tedOrInitialNotices = JsoupUtils.select("div.detail-avis.avis-reference > div > p:has(a)",
                document);
        for (Element tedOrInitialNotice : tedOrInitialNotices) {
            final Element tedOrInitialNoticeLink = JsoupUtils.selectFirst("a", tedOrInitialNotice);
            final String hrefAttribute = JsoupUtils.selectAttribute("href", tedOrInitialNoticeLink);
            if (hrefAttribute.startsWith(PublicationSources.EU_TED)) {
                final String textBeforeDate = "annonce diffusée le";
                String tedOwnText = tedOrInitialNotice.ownText();
                publicationDate = tedOwnText.substring(tedOwnText.indexOf(textBeforeDate)
                        + textBeforeDate.length()).trim();
                addPublicationTo(parsedTender, false, tedOrInitialNoticeLink.ownText(), PublicationSources.EU_TED,
                        hrefAttribute, publicationDate);
            } else {
                final String link = PublicationSources.FR_BOAMP_WEB + hrefAttribute;
                final String textBeforeDate = "Annonce publiée le";
                String noticeOwnText = tedOrInitialNotice.ownText();
                publicationDate = noticeOwnText.substring(
                        noticeOwnText.indexOf(textBeforeDate) + textBeforeDate.length(),
                        noticeOwnText.indexOf('-')).trim();
                final String sourceId = tedOrInitialNoticeLink.ownText().replace("Référence :", "").trim();
                addPublicationTo(parsedTender, false, sourceId, PublicationSources.FR_BOAMP_WEB,
                        link, publicationDate);
            }
        }

        // parse from "Informations pratiques" section
        String selectionMethod = JsoupUtils.selectText("h3:containsOwn(Critères d\\'attribution) + p", document);
        if (selectionMethod == null) {
            Element selectionMethodTitleNode = JsoupUtils.selectFirst("i:containsOwn(Critères d\\'attribution)", document);
            if (selectionMethodTitleNode != null && selectionMethodTitleNode.nextSibling() != null) {
                selectionMethod = selectionMethodTitleNode.nextSibling().toString();
            }
        }
        if (selectionMethod == null) {
            selectionMethod = getStringFromtText(new String[]{"IV.2.1", "Critères d\\'attribution"}, document);
        }


        // Notice
        parsedTender
                .setTitle(JsoupUtils.selectText("i:containsOwn(Objet du marché) + *", document))
                .setDescription(getStringFromtText("Quantité ou étendue :", document))
                .setDocuments(Arrays.asList(new ParsedDocument()
                        .setUrl(getStringFromtText(new String[]{
                                "Adresse internet :",
                                "Mairie de Plérin."},
                                document))
                ))
                .addCpv(new ParsedCPV()
                        .setCode(getStringFromtText("Classification CPV", document))
                        .setIsMain(Boolean.TRUE.toString())
                )
                .setAreVariantsAccepted(getStringFromtText("Division en lots", document))
                .setIsFrameworkAgreement(getStringFromtText("Marché couvert par l'accord sur les marchés publics", document))
                .setBuyerAssignedId(getStringFromtText("uméro de référence attribué au marché", document))
                .setEligibilityCriteria(getStringFromtText("Autres renseignements demandé", document))
                .setEstimatedStartDate(getStringFromtText("Durée du marché ou délai d'exécution", document))
                .setBidDeadline(JsoupUtils.selectText("p.date-response", document, true)
                    .replace("Date limite de réponse :", ""))
                .setSelectionMethod(selectionMethod)
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(getStringFromtText("ieu principal de livraison", document))
                        .addNuts(getStringFromtNode("Code NUTS", document))
                )
                .setAwardCriteria(parseAwardCriteria(document));

        // Award
        final String bidder = getStringFromtText("Nom du titulaire", document);

        if (bidder != null) {
            final String bidderName = bidder.split(",")[0];
            parsedTender
                    .addLot(new ParsedTenderLot()
                            .addBid(new ParsedBid()
                                    .addBidder(new ParsedBody()
                                            .setName(bidderName)
                                            .setAddress(new ParsedAddress()
                                                    .setRawAddress(bidder.replace(bidderName + ",", ""))
                                            )
                                    )
                                    .setPrice(new ParsedPrice()
                                            .setNetAmount(getStringFromtText("Montant", document))
                                            .setCurrency("EUR")
                                    )
                            )
                    )
                    .setFinalPrice(new ParsedPrice()
                            .setNetAmount(getStringFromtText("Montant", document))
                            .setCurrency("EUR")
                    );
        }
    }

    /**
     * Parse award criteria.
     *
     * @param document document
     * @return List<ParsedAwardCriterion> or null
     */
    private static List<ParsedAwardCriterion> parseAwardCriteria(final Document document) {
        final List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();

        final Element header = JsoupUtils.selectFirst("*:containsOwn(Critères d\\'attribution retenu)", document);

        if (header != null) {
            final List<Node> headerSiblings = header.parent().childNodes();

            boolean nextIsResult = false;
            for (Node rawSibling : headerSiblings) {
                final String sibling = textFromNode(rawSibling).trim();

                if (nextIsResult && sibling.startsWith("-")) {
                    final String[] nameAndWeight = sibling.split(":");

                    parsedCriteria.add(new ParsedAwardCriterion()
                            .setName(nameAndWeight[0])
                            .setWeight(nameAndWeight.length > 1 ? nameAndWeight[1] : null)
                            .setIsPriceRelated(String.valueOf(sibling.contains("prix")))
                    );
                }

                if (sibling.contains("Critères d'attribution retenu")) {
                    nextIsResult = true;
                }
            }
        }

        return parsedCriteria.isEmpty() ? null : parsedCriteria;
    }

    /**
     * This method adds publication information to tender. If the publication is already in the list, then the
     * information are merged to the publication. Otherwise new publication is created.
     *
     * @param parsedTender     tender to add data to
     * @param isIncluded       whether the publication is included
     * @param sourceId         publication source ID
     * @param source           publication source
     * @param humanReadableUrl HTML page URL
     * @param publicationDate  HTML page URL
     */
    private static void addPublicationTo(final ParsedTender parsedTender,
                                         final boolean isIncluded,
                                         final String sourceId,
                                         final String source,
                                         final String humanReadableUrl,
                                         final String publicationDate) {
        // get already existing publication or create new one
        assert parsedTender.getPublications()
                .stream()
                .filter(p -> p.getSourceId().equals(sourceId))
                .collect(Collectors.toList()).size() <= 1;
        ParsedPublication publicationToUpdate = parsedTender.getPublications() == null
                ? null
                : parsedTender.getPublications()
                .stream()
                .filter(p -> p.getSourceId().equals(sourceId))
                .findFirst()
                .orElse(null);
        if (publicationToUpdate == null) {
            publicationToUpdate = new ParsedPublication();
            parsedTender
                    .addPublication(publicationToUpdate);
        }

        // update the publication
        publicationToUpdate
                .setIsIncluded(isIncluded)
                .setSourceId(sourceId)
                .setSource(source)
                .setHumanReadableUrl(humanReadableUrl)
                .setPublicationDate(publicationDate == null
                        ? publicationToUpdate.getPublicationDate()
                        : publicationDate);
    }

    /**
     * In element with multiple text nodes, find one by selector and return following.
     *
     * @param selector selector
     * @param element  element
     *
     * @return String or null
     */
    private static String getStringFromtText(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final Element header = JsoupUtils.selectFirst("*:containsOwn(" + escapeSelectorSpecialChars(selector) + ")", element);
        if (header != null) {

            List<Node> headerSiblings = header.childNodes();

            boolean nextIsResult = false;
            for (Node rawSibling : headerSiblings) {
                final String sibling = textFromNode(rawSibling);

                if (nextIsResult && !sibling.trim().isEmpty()) {
                    return sibling;
                }

                if (sibling.contains(selector)) {
                    nextIsResult = true;
                }
            }

            headerSiblings = header.parent().childNodes();

            for (Node rawSibling : headerSiblings) {
                final String sibling = textFromNode(rawSibling);

                if (nextIsResult && !sibling.trim().isEmpty()) {
                    return sibling;
                }

                if (sibling.contains(selector)) {
                    nextIsResult = true;
                }
            }
        }

        return null;
    }

    /**
     * In element with multiple text nodes, find one by selector and return following.
     *
     * @param selectors selectors
     * @param element  element
     *
     * @return String or null
     */
    private static String getStringFromtText(final String[] selectors, final Element element) {
        if (element == null) {
            return null;
        }

        for (String selector : selectors) {
            final String result = getStringFromtText(selector, element);

            if (result != null) {
                return result;
            }
        }

        return null;
    }

    /**
     * Escape selector special characters.
     *
     * @param selector selector
     *
     * @return String
     */
    private static String escapeSelectorSpecialChars(final String selector) {
        return selector.replace("'", "\\'");
    }

    /**
     * Get text value from node.
     *
     * @param node node
     * @return String
     */
    private static String textFromNode(final Node node) {
        if (node instanceof Element) {
            return  ((Element) node).text();
        } else {
            return node.toString();
        }
    }

    /**
     * In element with multiple text nodes, find one by selector and return following.
     *
     * @param selector selector
     * @param element  element
     *
     * @return String or null
     */
    private static String getStringFromtNode(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final Element header = JsoupUtils.selectFirst("*:containsOwn(" + escapeSelectorSpecialChars(selector) + ")", element);
        if (header != null) {
            final List<Node> headerSiblings = header.childNodes();

            for (Node rawSibling : headerSiblings) {
                final String node = textFromNode(rawSibling);

                if (node.contains(selector)) {
                    return node;
                }
            }
        }

        return null;
    }
}
