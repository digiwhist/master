package eu.digiwhist.worker.si.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Tender parser for E-narocanje in Slovenia.
 *
 * @author Marek Mikes
 */
public class ENarocanjeTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1";

    private static final String SOURCE_DOMAIN = PublicationSources.SI_ENAROCANJE;

    /**
     * It is HTML character entity "&nbsp;".
     */
    static final Character NBSP_CHARACTER = (char) 160;

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        // get form element from the page
        final Document page = Jsoup.parse(rawTender.getSourceData());
        final Elements forms = JsoupUtils.select("div.col-md-10 > div.panel.panel-default:has(" +
                "div.panel-heading.panel-title > span > span.glyphicon.glyphicon-list-alt)", page);
        if (forms.size() != 1) {
            logger.error("Page or selector is incorrect.");
            throw new UnrecoverableException("Page or selector is incorrect.");
        }
        final Element form = forms.get(0);

        ParsedTender tender = new ParsedTender();

        // set common attributes (basic information of publications)
        // we get related publication table from the whole page, because it is not in the form
        Elements relatedPublicationsTables = JsoupUtils.select("table.table.table-bordered.table-hover", page);
        if (relatedPublicationsTables.isEmpty()) {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=206778
            tender
                    .addPublication(new ParsedPublication()
                            .setIsIncluded(true)
                            .setHumanReadableUrl(rawTender.getSourceUrl().toString())
                            .setSource(SOURCE_DOMAIN)
                            .setSourceId(parsePublicationSourceId(form))
                            .setSourceFormType(parsePublicationSourceFormType(form)));
        } else {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=207586
            assert relatedPublicationsTables.size() == 1
                    : "The related publication table has to be just once on the page";
            final List<ParsedPublication> publications = new ArrayList<>();
            for (Element row : JsoupUtils.select("tr", relatedPublicationsTables.first())) {
                final String onclickAttributeValue = JsoupUtils.selectAttribute("onclick", row);
                if (!onclickAttributeValue.startsWith("location.href='?id_obrazec=")) {
                    // we do not want this publication link. It is link to some document (contract or decision)
                    // e.g. location.href='?id_odlocitve=12419&id_obrazec=207586'
                    continue;
                }
                // we want this publication link

                // get human readable URL
                assert onclickAttributeValue.endsWith("'");
                String humanReadableUrl = PublicationSources.SI_ENAROCANJE + "/Obrazci/"
                        + onclickAttributeValue.substring(onclickAttributeValue.indexOf("'") + 1,
                        onclickAttributeValue.length() - 1);

                final String rowText = row.text();
                // we want get last occurrences of parentheses. See https://www.enarocanje.si/Obrazci/?id_obrazec=145732
                assert rowText.contains("(") && rowText.contains(")")
                        && rowText.lastIndexOf('(') < rowText.lastIndexOf(')');
                final ParsedPublication publication = new ParsedPublication()
                        .setHumanReadableUrl(humanReadableUrl)
                        .setSource(SOURCE_DOMAIN)
                        .setSourceId(rowText.substring(0, rowText.indexOf(NBSP_CHARACTER)))
                        .setSourceFormType(rowText.substring(rowText.lastIndexOf('(') + 1, rowText.lastIndexOf(')'))
                                .trim());

                // included publication is first in the list of publications
                if (JsoupUtils.selectAttribute("class", row).equals("active")) {
                    assert publication.getHumanReadableUrl().equals(rawTender.getSourceUrl().toString());
                    assert publication.getSourceId().equals(parsePublicationSourceId(form));
                    assert publication.getSourceFormType().equals(parsePublicationSourceFormType(form));
                    publication
                            .setIsIncluded(true);
                    publications.add(0, publication);
                } else {
                    publication
                            .setIsIncluded(false);
                    publications.add(publication);
                }
            }
            tender
                    .setPublications(publications);
        }

        // parse form specific attributes
        final String sourceFormType = tender.getPublications().get(0).getSourceFormType();
        switch (sourceFormType) {
            case "EU 2 - SL":
            case "EU 5 - SL":
            case "NMV1":
                // the form can have old or new structure
                if (isNewForm(form)) {
                    tender = ENarocanjeContractNoticeHandler1New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractNoticeHandler1Old.parse(tender, form);
                }
                break;
            case "PZPPO1 - ZJNVETPS":
            case "PZPPO1 - ZJN-2":
                tender = ENarocanjeContractNoticeHandler2.parse(tender, form);
                break;
            case "PZP":
                tender = ENarocanjeContractNoticeHandler3.parse(tender, form);
                break;
            case "EU 3 - SL":
            case "EU 6 - SL":
            case "NMV2":
                // the form can have old or new structure
                if (isNewForm(form)) {
                    tender = ENarocanjeContractAwardHandler1New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractAwardHandler1Old.parse(tender, form);
                }
                break;
            case "EU 18 - SL":
            case "PZPPO2 - ZJN-2":
            case "PZPPO2 - ZJNVETPS":
                tender = ENarocanjeContractAwardHandler2.parse(tender, form);
                break;
            case "OS - ZJN-2":
            case "OS - ZJNVETPS":
                tender = ENarocanjeContractImplementation.parse(tender, form);
                break;
            default:
                logger.warn("No handler found for form code: {}", sourceFormType);
                break;
        }

        return Arrays.asList(tender);
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "SI";
    }

    /**
     * Parse publication source ID value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceId(final Element form) {
        final Element formHead = JsoupUtils.selectFirst("div.panel-heading.panel-title > span", form);
        final String formHeadOwnText = formHead.ownText().replace(NBSP_CHARACTER, ' ').trim();
        // the own text contains just the source ID or the source ID and TED reference separated by space
        return formHeadOwnText.contains(" ") ? formHeadOwnText.split(" ")[0] : formHeadOwnText;
    }

    /**
     * Parse publication source form type value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceFormType(final Element form) {
        final String sourceFormTypeTextAndCode = JsoupUtils.selectText("div.panel-heading.panel-title > span > small",
                form);
        assert sourceFormTypeTextAndCode.contains("(") && sourceFormTypeTextAndCode.contains(")")
                && sourceFormTypeTextAndCode.lastIndexOf('(') < sourceFormTypeTextAndCode.lastIndexOf(')')
                && sourceFormTypeTextAndCode.lastIndexOf(')') == sourceFormTypeTextAndCode.length() - 1;
        return sourceFormTypeTextAndCode.substring(sourceFormTypeTextAndCode.lastIndexOf('(') + 1,
                sourceFormTypeTextAndCode.lastIndexOf(')')).trim();
    }

    /**
     * Says whether the form is new or old.
     *
     * @param form
     *         document to be parsed
     *
     * @return true when the form is new, otherwise false
     */
    private static boolean isNewForm(final Element form) {
        return JsoupUtils.exists(BaseENarocanjeFormInDivsHandler.SUBSECTION_I_1_CONTENT_SELECTOR, form);
    }

}
