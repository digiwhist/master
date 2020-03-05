package eu.datlab.worker.id.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Arrays;

/**
 * Tender cleaner for Indonesia.
 *
 * @author Marek Mikes
 */
public class LPSETenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("id");

    private static final NumberFormat NUMBER_FORMAT;

    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator('.');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern("d MMMM uuuu[ H:m]", LOCALE);

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender, final CleanTender cleanTender) {
        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("bodies", new BodyPlugin(null, null, null))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, Collections.emptyMap()))
                .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, getFormTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping(), null))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(getSelectionMethodMapping()))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("awardCriterion", new AwardCriteriaPlugin(NUMBER_FORMAT));
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("CONTRACT_AWARD"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("CONTRACT_NOTICE"));
        return mapping;
    }

    /**
     * @return supply type mapping
     */
    private Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Pekerjaan Konstruksi"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Pengadaan Barang"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Jasa Konsultansi Badan Usaha",
                "Jasa Lainnya", "Jasa Konsultansi Perorangan"));
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("APBN", "APBD", "APBDP", "PNBP",
                "APBDPAPBDAPBDAPBDPAPBDAPBDAPBDPAPBDAPBDAPBDAPBDAPBDAPBDAPBDAPBDAPBDAPBDPAPBDAPBDAPBDAPBD", "APBN"));
        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList(
                "e-Lelang Pemilihan Langsung",
                "e-Seleksi Langsung",
                "e-Penunjukan Langsung",
                "Lelang Pemilihan Langsung - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur"
        ));

        mapping.put(TenderProcedureType.OPEN, Arrays.asList(
                "e-Lelang Umum",
                "e-Lelang Sederhana",
                "e-Seleksi Umum",
                "Lelang Umum - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Sederhana - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Seleksi Umum - Prakualifikasi Satu File - Biaya Terendah",
                "Seleksi Umum - Prakualifikasi Satu File - Pagu Anggaran",
                "Seleksi Umum - Prakualifikasi Dua File - Pagu Anggaran",
                "Lelang Umum - Prakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Prakualifikasi Dua File - Kualitas",
                "Lelang Umum - Pascakualifikasi Dua File - Sistem Nilai",
                "Seleksi Umum - Pascakualifikasi Satu File - Kualitas",
                "Lelang Umum - Pascakualifikasi Satu File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Lelang Umum - Prakualifikasi Dua Tahap - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Umum - Pascakualifikasi Dua File - Sistem Umur Ekonomis",
                "Lelang Sederhana - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Lelang Umum - Prakualifikasi Satu File - Biaya Terendah",
                "Lelang Sederhana - Prakualifikasi Satu File - Biaya Terendah",
                "Lelang Umum - Prakualifikasi Satu File - Pagu Anggaran",
                "Lelang Umum - Prakualifikasi Dua File - Kualitas",
                "Seleksi Umum - Prakualifikasi Dua File - Biaya Terendah",
                "Seleksi Umum - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Sederhana - Prakualifikasi Dua File - Pagu Anggaran",
                "Lelang Sederhana - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Harga Terendah Sistem Gugur",
                "Lelang Sederhana - Pascakualifikasi Satu File - Kualitas",
                "Lelang Sederhana - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur"
                ));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList(
                "e-Seleksi Sederhana",
                "e-Lelang Terbatas",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Pagu Anggaran",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Biaya Terendah",
                "Seleksi Sederhana - Prakualifikasi Satu File - Biaya Terendah",
                "Seleksi Sederhana - Prakualifikasi Satu File - Pagu Anggaran",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Kualitas",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Terbatas - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur",
                "Lelang Terbatas - Prakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Terbatas - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Sederhana - Prakualifikasi Dua File - Kualitas",
                "Lelang Terbatas - Prakualifikasi Dua File - Sistem Nilai",
                "Seleksi Sederhana - Prakualifikasi Dua File - Kualitas dan Biaya"
        ));

        mapping.put(TenderProcedureType.OTHER, Arrays.asList(
                "e-Lelang Cepat",
                "e-Seleksi Cepat",
                "Tender Cepat - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Cepat - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Cepat - Pascakualifikasi Satu File - Kualitas",
                "Seleksi Cepat - Pascakualifikasi Satu File - Biaya Terendah",
                "Tender Cepat - Prakualifikasi Dua File - Sistem Nilai"
                ));


        return mapping;
    }

    /**
     * @return selection method mapping
     */
    private static Map<Enum, List<String>> getSelectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList(
                "Lelang Pemilihan Langsung - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Umum - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Sederhana - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Biaya Terendah",
                "Seleksi Sederhana - Prakualifikasi Satu File - Biaya Terendah",
                "Tender Cepat - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Prakualifikasi Satu File - Biaya Terendah",
                "Tender - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Umum - Prakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Cepat - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Tender - Pascakualifikasi Dua File - Harga Terendah Ambang Batas",
                "Lelang Umum - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi - Prakualifikasi Dua File - Biaya Terendah",
                "Lelang Terbatas - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur",
                "Lelang Umum - Prakualifikasi Satu File - Biaya Terendah",
                "Lelang Terbatas - Prakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Sederhana - Prakualifikasi Satu File - Biaya Terendah",
                "Seleksi - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Lelang Terbatas - Pascakualifikasi Satu File - Harga Terendah Sistem Gugur",
                "Seleksi Umum - Prakualifikasi Dua File - Biaya Terendah",
                "Lelang Umum - Prakualifikasi Dua File - Harga Terendah Sistem Gugur",
                "Seleksi - Prakualifikasi Dua Tahap - Biaya Terendah",
                "Seleksi Cepat - Pascakualifikasi Satu File - Biaya Terendah",
                "Lelang Sederhana - Prakualifikasi Dua Tahap - Harga Terendah Sistem Gugur"
                ));
        mapping.put(SelectionMethod.MEAT, Arrays.asList(
                "Seleksi Umum - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Seleksi Sederhana - Pascakualifikasi Satu File - Kualitas",
                "Seleksi Umum - Prakualifikasi Dua File - Kualitas",
                "Lelang Umum - Pascakualifikasi Dua File - Sistem Nilai",
                "Seleksi Umum - Pascakualifikasi Satu File - Kualitas",
                "Lelang Umum - Pascakualifikasi Satu File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Seleksi - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Lelang Umum - Prakualifikasi Dua Tahap - Sistem Nilai",
                "Lelang Umum - Pascakualifikasi Dua File - Sistem Umur Ekonomis",
                "Lelang Sederhana - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Seleksi - Pascakualifikasi Dua File - Kualitas",
                "Seleksi - Prakualifikasi Dua File - Sistem Nilai",
                "Tender - Pascakualifikasi Dua File - Sistem Nilai",
                "Lelang Umum - Prakualifikasi Dua File - Kualitas",
                "Seleksi - Prakualifikasi Satu File - Kualitas dan Biaya",
                "Seleksi Sederhana - Prakualifikasi Dua File - Kualitas",
                "Seleksi Umum - Prakualifikasi Dua File - Sistem Nilai",
                "Seleksi - Prakualifikasi Dua File - Kualitas",
                "Lelang Sederhana - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Terbatas - Prakualifikasi Dua File - Sistem Nilai",
                "Lelang Sederhana - Pascakualifikasi Satu File - Kualitas",
                "Seleksi Sederhana - Prakualifikasi Dua File - Kualitas dan Biaya",
                "Seleksi Cepat - Pascakualifikasi Satu File - Kualitas",
                "Tender Cepat - Prakualifikasi Dua File - Sistem Nilai"
        ));

        return mapping;
    }

}
