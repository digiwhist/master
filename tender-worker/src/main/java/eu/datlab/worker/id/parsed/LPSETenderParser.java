package eu.datlab.worker.id.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tender parser for Indonesia.
 *
 * @author Tomas Mrazek
 */
public class LPSETenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    /**
     * Snippet type enumeration.
     */
    enum LPSESnippetType {
        /**
         * Notice snippet.
         */
        NOTICE("pengumumanlelang", (d, t) -> {
            String procedureType = JsoupUtils.getFirstValueByLabel(d, "Metode Pengadaan");
            if(procedureType == null || procedureType.isEmpty()){
                procedureType = JsoupUtils.getFirstValueByLabel(d, "Sistem Pengadaan");
            }

            t.setTitle(JsoupUtils.getFirstValueByLabel(d, "Nama (Lelang|Tender)"))
                    .addBuyer(new ParsedBody().setName(JsoupUtils.getFirstValueByLabel(d, "Instansi")))
                    .setSupplyType(JsoupUtils.getFirstValueByLabel(d, "Kategori"))
                    .setNationalProcedureType(procedureType)
                    .setProcedureType(procedureType)
                    .addAwardCriterion(new ParsedAwardCriterion()
                            .setName("Bobot Teknis")
                            .setWeight(JsoupUtils.getFirstValueByLabel(d, "Bobot Teknis"))
                            .setIsPriceRelated(Boolean.FALSE.toString()))
                    .addAwardCriterion(new ParsedAwardCriterion()
                            .setName("Bobot Biaya")
                            .setWeight(JsoupUtils.getFirstValueByLabel(d, "Bobot Biaya"))
                            .setIsPriceRelated(Boolean.TRUE.toString()))
                    .setEligibilityCriteria(JsoupUtils.getFirstValueByLabel(d, "Syarat Kualifikasi"))
                    .setSelectionMethod(JsoupUtils.getFirstValueByLabel(d, "Sistem Pengadaan"));

            String sourceId = JsoupUtils.getFirstValueByLabel(d, "Kode (Lelang|Tender)");
            t.getPublications().get(0)
                    .setIsIncluded(true)
                    .setSource(PublicationSources.ID_LPSE)
                    .setSourceId(sourceId)
                    .setSourceFormType(PublicationFormType.CONTRACT_AWARD.name());

            t.addPublication(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.ID_LPSE)
                    .setSourceId(sourceId)
                    .setSourceFormType(PublicationFormType.CONTRACT_NOTICE.name())
                    .setPublicationDate(JsoupUtils.getFirstValueByLabel(d, "Tanggal Pembuatan")));


            String isAwarded = JsoupUtils.getFirstValueByLabel(d, "Tahap (Lelang|Tender) Saat ini");
            if (isAwarded != null) {
                t.setIsAwarded(String.valueOf(isAwarded.matches("(Lelang|Tender) Sudah Selesai")));
            }

            t.getLots().get(0).setBidsCount(JsoupUtils.getFirstValueByLabel(d, "Peserta (Lelang|Tender)"));

            return t;
        }),
        /**
         * Participants snippet.
         */
        PARTICIPANTS("peserta", (d, t) -> {
            ParsedTenderLot lot = t.getLots().get(0);

            Elements rows = JsoupUtils.select("table.table-condensed > tbody > tr", d);
            for (Element r : rows) {
                lot.addBid(new ParsedBid()
                        .addBidder(new ParsedBody()
                                .setName(getChildText(r, 1))
                                .addBodyId(parseBodyId(getChildText(r, 2))))
                        .setPrice(parsePrice(getChildText(r, 3))));
            }

            return t;
        }),
        /**
         * Results snippet.
         */
        RESULTS("hasil", (d, t) -> {
            ParsedTenderLot lot = t.getLots().get(0);
            Elements header = JsoupUtils.select("table.table-condensed > thead > tr", d);
            Elements rows = JsoupUtils.select("table.table-condensed > tbody > tr", d);
            int pColumn = 0, kColumn = 0, priceColumn = 0, reasonColumn = 0;
            for (int i = 1; i < header.get(0).childNodeSize(); i += 2) {
                if (header.get(0).childNode(i).childNodeSize() > 0
                        && header.get(0).childNode(i).childNode(0).childNodeSize() > 0) {
                    if (header.get(0).childNode(i).childNode(0).childNode(0).toString().equals("P")) {
                        pColumn = i;
                    } else if (header.get(0).childNode(i).childNode(0).childNode(0).toString().equals("K")) {
                        kColumn = i;
                    }
                } else if (header.get(0).childNode(i).childNode(0).toString().equals("Penawaran Terkoreksi")) {
                    priceColumn = i;
                } else if (header.get(0).childNode(i).childNode(0).toString().equals("Alasan")) {
                    reasonColumn = i;
                }
            }

            final int pColumnNumber = pColumn;
            final int kColumnNumber = kColumn;
            final int priceColumnNumber = priceColumn;
            final int reasonColumnNumber = reasonColumn;

            // update bidders and its bids
            rows.stream()
                    // checked in 'K' column
                    .filter(r -> r.childNode(kColumnNumber).childNodeSize() != 0)
                    .forEach(r -> {
                        // get bid to be updated
                        ParsedBid bid = Optional.ofNullable(lot.getBids()).orElse(Collections.emptyList()).stream()
                                .filter(b -> {
                                    ParsedBody parsedBidder = b.getBidders().get(0);

                                    String bidder = getChildText(r, 1);

                                    return (bidder.startsWith(parsedBidder.getName().trim())
                                            && bidder.endsWith(parsedBidder.getBodyIds().get(0).getId()))
                                            || parsedBidder.getName().trim().contains("Peserta");
                                }).findFirst().orElse(null);

                        if (bid == null) {
                            return;
                        }

                        if (bid.getBidders().get(0).getName().contains("Peserta")) {
                            bid.getBidders().get(0)
                                    .setName(getChildText(r, 1).split(" - ")[0])
                                    .addBodyId(parseBodyId(getChildText(r, 1).split(" - ")[1]));
                        }

                        if (r.childNode(priceColumnNumber).childNodeSize() != 0) {
                            bid.setPrice(parsePrice(r.childNode(priceColumnNumber).childNode(0).toString()));
                        }
                        if (r.childNode(reasonColumnNumber).childNodeSize() != 0) {
                            bid.setDisqualificationReason(r.childNode(reasonColumnNumber).childNode(0).toString());
                        }

                        // star in 'P' column marks winner
                        if (pColumnNumber != 0 && r.childNode(pColumnNumber).childNodeSize() != 0) {
                            bid.setIsWinning(Boolean.TRUE.toString());
                        }

                    });

            return t;
        }),
        /**
         * Winner snippet.
         */
        WINNER("pemenang", (d, t) -> {
            ParsedTenderLot lot = t.getLots().get(0);

            t.setEstimatedPrice(parsePrice(JsoupUtils.getFirstValueByLabel(d, "Pagu")));
            // already in NOTICE
            if (t.getTitle() == null) {
                t.setTitle(JsoupUtils.getFirstValueByLabel(d, "Nama (Lelang|Tender)"));
            }
            if (t.getSupplyType() == null) {
                t.setSupplyType(JsoupUtils.getFirstValueByLabel(d, "Ketagori"));
            }
            if (t.getBuyers() != null && !t.getBuyers().isEmpty() && t.getBuyers().get(0).getName() == null) {
                t.getBuyers().get(0).setName(JsoupUtils.getFirstValueByLabel(d, "Agency"));
            }

            ParsedBid winningBid = Optional.ofNullable(lot.getBids()).orElse(Collections.emptyList()).stream()
                    .filter(n -> Boolean.TRUE.equals(n.getIsWinning()))
                    .findFirst().orElse(null);

            ParsedBody winner = winningBid != null ? winningBid.getBidders().get(0) : null;

            // update winner data
            Element winnerNode = JsoupUtils.selectFirst("table.table-condensed table.table-condensed > tbody > tr:eq(1)", d);
            if (winnerNode != null) {
                if (winner != null) {
                    if (winner.getName() == null) {
                        winner.setName(getChildText(winnerNode, 0));
                    }

                    if (winner.getAddress() == null) {
                        String rawAddress = getChildText(winnerNode, 1);
                        ParsedAddress address = new ParsedAddress().setRawAddress(rawAddress);

                        Matcher m = Pattern.compile("(?<street>[^\\-]+) \\- (?<city>[^\\-]+) \\- (?<state>[^\\-]+)")
                                .matcher(rawAddress);
                        if (m.find()) {
                            winner
                                    .setAddress(address.setStreet(m.group("street"))
                                            .setCity(m.group("city"))
                                            .setState(m.group("state")));
                        }
                    }

                    if (winner.getBodyIds() == null) {
                        winner.addBodyId(parseBodyId(getChildText(winnerNode, 2)));
                    }
                }

                if (winningBid != null && winningBid.getPrice() == null) {
                    winningBid.setPrice(parsePrice(getChildText(winnerNode, 3)));
                }
            }

            return t;
        }),
        /**
         * Winner contract snippet.
         */
        WINNER_CONTRACT("pemenangberkontrak", WINNER.parser),
        /**
         * Plan snippet.
         */
        PLAN("jadwal", (d, t) -> {
            ParsedTenderLot lot = t.getLots().get(0);

            Elements rows = JsoupUtils.select("table.table-condensed > tbody > tr", d);

            for (Element r : rows) {
                String date = getChildText(r, 3);

                switch (getChildText(r, 1)) {
                    case "Download Dokumen Kualifikasi":
                        t.setDocumentsDeadline(date);
                        break;
                    case "Upload Dokumen Penawaran":
                        t.setBidDeadline(date);
                        break;
                    case "Penetapan Pemenang":
                        lot.setAwardDecisionDate(date);
                        break;
                    case "Pengumuman Pemenang":
                        t.getPublications().get(0).setPublicationDate(date);
                        break;
                    case "Penandatanganan Kontrak":
                        lot.setContractSignatureDate(date);
                        break;
                    default:
                        continue;
                }
            }
            return t;
        });

        private final String name;

        private final BiFunction<Document, ParsedTender, ParsedTender> parser;

        /**
         * @param name   name of tab used in url
         * @param parser parser which updates input tender with snippet specific data
         */
        LPSESnippetType(final String name, final BiFunction<Document, ParsedTender, ParsedTender> parser) {
            this.name = name;
            this.parser = parser;
        }

        /**
         * @param url url of the snippet
         * @return appropriate type
         */
        public static final LPSESnippetType fromUrl(final String url) {
            if (url != null) {
                for (LPSESnippetType n : LPSESnippetType.values()) {
                    if (url.endsWith(n.name)) {
                        return n;
                    }
                }
            }

            return null;
        }

        /**
         * @param document document to be parsed
         * @param tender   parsed tender
         * @return parsed tender
         */
        public final ParsedTender parse(final Document document, final ParsedTender tender) {
            return parser.apply(document, tender);
        }
    }

    /**
     * @param parent parsed parent element
     * @param n      child index
     * @return text of the n-th child's content or NULL if the given ingex is out of bounds.
     */
    private static String getChildText(final Element parent, final int n) {
        if (parent == null || n < 0) {
            return null;
        }

        try {
            return parent.child(n).text();
        } catch (IndexOutOfBoundsException ex) {
            return null;
        }
    }

    /**
     * @param input string to be parsed
     * @return parsed price or null
     */
    private static ParsedPrice parsePrice(final String input) {
        if (input == null) {
            return null;
        }

        String[] price = input.split(" ");
        if (price.length < 2) {
            return null;
        }

        return new ParsedPrice().setNetAmount(price[1]).setCurrency(price[0]);
    }

    /**
     * @param input string to be parsed
     * @return parsed body identifier or null
     */
    private static BodyIdentifier parseBodyId(final String input) {
        if (input == null) {
            return null;
        }

        return new BodyIdentifier()
                .setId(input)
                .setScope(BodyIdentifier.Scope.ID)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID);
    }

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        String url = rawTender.getSourceUrl().toString();

        HashMap metaData = rawTender.getMetaData();
        Map<String, String> additionalUrls = (Map<String, String>) metaData.get("additionalUrls");

        Map<LPSESnippetType, Document> snippets = new HashMap<>();
        snippets.put(LPSESnippetType.NOTICE, Jsoup.parse(rawTender.getSourceData()));

        for (Map.Entry<String, String> e : additionalUrls.entrySet()) {
            LPSESnippetType type = LPSESnippetType.fromUrl(e.getKey());
            Document snippet = Jsoup.parse(e.getValue());

            Element content = snippet.selectFirst("div.content > table.table-condensed > tbody");
            if (content != null) {
                snippets.put(type, snippet);
            }
        }

        ParsedTender tender = new ParsedTender()
                // lot and publication initialization because of easier parsing, no test of existence is needed
                .addLot(new ParsedTenderLot())
                .addPublication(new ParsedPublication());


        tender = snippets.get(LPSESnippetType.NOTICE) == null ? tender : LPSESnippetType.NOTICE.parse(
                snippets.get(LPSESnippetType.NOTICE), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.NOTICE.name());

        tender = snippets.get(LPSESnippetType.PARTICIPANTS) == null ? tender : LPSESnippetType.PARTICIPANTS.parse(
                snippets.get(LPSESnippetType.PARTICIPANTS), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.PARTICIPANTS.name());

        tender = snippets.get(LPSESnippetType.RESULTS) == null ? tender : LPSESnippetType.RESULTS.parse(
                snippets.get(LPSESnippetType.RESULTS), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.RESULTS.name());

        tender = snippets.get(LPSESnippetType.PLAN) == null ? tender : LPSESnippetType.PLAN.parse(
                snippets.get(LPSESnippetType.PLAN), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.PLAN.name());

        tender = snippets.get(LPSESnippetType.WINNER) == null ? tender : LPSESnippetType.WINNER.parse(
                snippets.get(LPSESnippetType.WINNER), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.WINNER.name());

        tender = snippets.get(LPSESnippetType.WINNER_CONTRACT) == null ? tender : LPSESnippetType.WINNER_CONTRACT.parse(
                snippets.get(LPSESnippetType.WINNER_CONTRACT), tender);
        logger.debug("Snippet {} parsed", LPSESnippetType.WINNER_CONTRACT.name());

        // set URL of included publication (parsed in NOTICE snippet parser)
        tender.getPublications().forEach(a -> a.setHumanReadableUrl(url));

        return Collections.singletonList(tender);
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "ID";
    }
}
