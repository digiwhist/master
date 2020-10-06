package eu.datlab.worker.si.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tender parser for E-narocanje in Slovenia.
 *
 * @author Marek Mikes
 */
public class ENarocanjeTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    private static final String SOURCE_DOMAIN = PublicationSources.SI_ENAROCANJE;

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        // fix broken table cells
        String sourceData = rawTender.getSourceData()
            .replaceAll("<tr><script>", "<tr><td><script>")
            .replace((char) 160, ' ')
            .replace("&nbsp;", " ");

        // get form element from the page
        final Document page = Jsoup.parse(sourceData);
        final Elements forms = JsoupUtils.select("div.col-md-10 > div.panel.panel-default:has(" +
                "div.panel-heading.panel-title > span > span.glyphicon.glyphicon-list-alt)", page);
        if (forms.size() != 1) {
            logger.error("Page or selector is incorrect.");
            throw new UnrecoverableException("Page or selector is incorrect.");
        }
        final Element form = forms.get(0);

        ParsedTender tender = new ParsedTender();

        Element publicationDateNode = JsoupUtils.selectFirst("center:eq(0) > font.naslovmali:containsOwn(Datum objave:)", form);
        String publicationDate = publicationDateNode != null
            ? publicationDateNode.childNode(0).toString() : (String) rawTender.getMetaData().get("publicationDate");

        // set common attributes (basic information of publications)
        // we get related publication table from the whole page, because it is not in the form
        Elements relatedPublicationsTables = JsoupUtils.select("table.table.table-bordered.table-hover", page);
        if (relatedPublicationsTables.isEmpty()) {
            tender.addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setHumanReadableUrl(rawTender.getSourceUrl().toString())
                .setSource(SOURCE_DOMAIN)
                .setSourceId(parsePublicationSourceId(form))
                .setSourceFormType(parsePublicationSourceFormType(form))
                .setPublicationDate(publicationDate));
        } else {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=207586
            assert relatedPublicationsTables.size() == 1 : "The related publication table has to be just once on the page";
            final List<ParsedPublication> publications = new ArrayList<>();
            for (Element row : JsoupUtils.select("tr", relatedPublicationsTables.first())) {
                final String onclickAttributeValue = JsoupUtils.selectAttribute("onclick", row);
                if (!onclickAttributeValue.startsWith("location.href='?id_obrazec=")) {
                    // we do not want this publication link. It is link to some document (contract or decision)
                    // e.g. location.href='?id_odlocitve=12419&id_obrazec=207586'
                    continue;
                }
                // get human readable URL
                assert onclickAttributeValue.endsWith("'");
                String humanReadableUrl = PublicationSources.SI_ENAROCANJE + "/Obrazci/"
                        + onclickAttributeValue.substring(onclickAttributeValue.indexOf("'") + 1, onclickAttributeValue.length() - 1);

                final String rowText = row.text().trim();
                // we want get last occurrences of parentheses. See https://www.enarocanje.si/Obrazci/?id_obrazec=145732
                assert rowText.contains("(") && rowText.contains(")") && rowText.lastIndexOf('(') < rowText.lastIndexOf(')');

                Matcher m = Pattern.compile("(?<sid>[^ ]+) .*\\((?<type>[^\\(\\)]+)\\).*, objavljeno dne"
                    + " (?<date>\\d{1,2}.\\d{1,2}.\\d{4})").matcher(rowText);
                String date = null, sourceId = null, sourceFormType = "";
                if (m.find()) {
                    sourceId = m.group("sid");
                    sourceFormType = m.group("type");
                    date = m.group("date");
                }

                final ParsedPublication publication = new ParsedPublication()
                    .setHumanReadableUrl(humanReadableUrl)
                    .setSource(SOURCE_DOMAIN)
                    .setSourceId(sourceId)
                    .setSourceFormType(sourceFormType)
                    .setPublicationDate(date);

                // included publication is first in the list of publications
                if (JsoupUtils.selectAttribute("class", row).equals("active")) {
                    assert publication.getHumanReadableUrl().equals(rawTender.getSourceUrl().toString());
                    assert publication.getSourceId().equals(parsePublicationSourceId(form));
                    assert publication.getSourceFormType().equals(parsePublicationSourceFormType(form));
                    
                    publication.setIsIncluded(true).setPublicationDate(publicationDate);                    
                    publications.add(0, publication);
                } else {
                    publication.setIsIncluded(false);
                    publications.add(publication);
                }
            }
            tender.setPublications(publications);
        }
        // update source form type when it is cancellation/correction (the form type contains "preklic/popravek")
        // to distinguish cancellation and correction. We append the first sentence in I.1
        if (tender.getPublications().get(0).getSourceFormType().contains("preklic/popravek")) {
            final String sectionI1Content = ENarocanjeTenderFormInTableUtils.getSectionContent("I.1", form,
                    Arrays.asList("TO OBVESTILO SE NANAŠA NA"));
            if (sectionI1Content != null) {
                tender.getPublications().get(0)
                    .setSourceFormType(tender.getPublications().get(0).getSourceFormType() + " - "
                        + sectionI1Content.split("\\.")[0]);
            }
        }

        // parse form specific attributes
        final String sourceFormType = tender.getPublications().get(0).getSourceFormType();
        switch (sourceFormType) {
            case "EU 2 - SL": case "EU 5 - SL":
                if (isNewForm(form)) {
                    tender = ENarocanjeContractNoticeHandler1New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractNoticeHandler1Old.parse(tender, form);
                }
                break;
            case "NMV1":
                if (isNewForm(form)) {
                    tender = ENarocanjeContractNoticeHandler4New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractNoticeHandler4Old.parse(tender, form);
                }
                break;
            case "PZPPO1 - ZJN-2": case "PZPPO1 - ZJNVETPS":
                tender = ENarocanjeContractNoticeHandler2.parse(tender, form);
                break;
            case "PZP":
                tender = ENarocanjeContractNoticeHandler3.parse(tender, form);
                break;
            case "EU 3 - SL":
                if (isNewForm(form)) {
                    tender = ENarocanjeContractAwardHandler1New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractAwardHandler1Old.parse(tender, form);
                }
                break;
            case "EU 6 - SL": case "NMV2":
                if (isNewForm(form)) {
                    tender = ENarocanjeContractAwardHandler3New.parse(tender, form);
                } else {
                    tender = ENarocanjeContractAwardHandler3Old.parse(tender, form);
                }
                break;
            case "EU 18 - SL": case "PZPPO2 - ZJN-2": case "PZPPO2 - ZJNVETPS":
                tender = ENarocanjeContractAwardHandler2.parse(tender, form);
                break;
            case "OS - ZJN-2": case "OS - ZJNVETPS":
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
        final String formHeadOwnText = formHead.ownText().trim();
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
        final String data = JsoupUtils.selectText("div.panel-heading.panel-title > span > small", form);
        if (data == null) {
            return null;
        }

        Matcher m = Pattern.compile(".*\\((?<type>[^\\(\\)]+)\\)").matcher(data);
        return m.find() ? m.group("type").trim() : null;
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
        return JsoupUtils.exists(BaseENarocanjeFormInDivsHandler.SUBSECTION_I_1_TITLE_SELECTOR + " + div", form);
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
