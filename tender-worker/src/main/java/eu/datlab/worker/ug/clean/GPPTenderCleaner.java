package eu.datlab.worker.ug.clean;

import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Tender cleaner for Uganda.
 *
 * @author Tomas Mrazek
 */
public class GPPTenderCleaner extends BaseGPPTenderCleaner {
    private static final String VERSION = "1.0";

    /**
     * Class for cpv code and rule.
     */
    private static class CpvCodeAndRule {
        private final String code;
        private final List<String> rule;

        /**
         * Constructor.
         * @param code cpv code
         * @param rule rule
         */
        CpvCodeAndRule(final String code, final List<String> rule) {
            this.code = code;
            this.rule = rule;
        }


        /**
         * Getter for code.
         * @return code
         */
        public String getCode() {
            return code;
        }

        /**
         * Getter for rule.
         * @return rule
         */
        public List<String> getRule() {
            return rule;
        }
    }

    /**
     * Class for city and nuts.
     */
    private static class CityAndNuts {
        private final String city;
        private final String nuts;

        /**
         * Constructor.
         * @param city city
         * @param nuts nuts
         */
        CityAndNuts(final String city, final String nuts) {
        this.city = city;
        this.nuts = nuts;
    }

    /**
     * Getter for city.
     * @return city
     */
    public String getCity() {
        return city;
    }

    /**
     * Getter for nuts.
     * @return nuts
     */
    public String getNuts() {
        return nuts;
    }
}

    private static final List<CpvCodeAndRule> CpvRules = Arrays.<CpvCodeAndRule>asList(
            new CpvCodeAndRule("341000008", Arrays.asList("motor vehicle")),
            new CpvCodeAndRule("555200001", Arrays.asList("catering & services")),
            new CpvCodeAndRule("501150004", Arrays.asList("motor & servic")),
            new CpvCodeAndRule("301920001", Arrays.asList("office & supply")),
            new CpvCodeAndRule("909192004", Arrays.asList("office & clean")),
            new CpvCodeAndRule("715200009", Arrays.asList("construction & supervision")),
            new CpvCodeAndRule("715000003", Arrays.asList("construction & staff")),
            new CpvCodeAndRule("452331206", Arrays.asList("construction & road")),
            new CpvCodeAndRule("300000009", Arrays.asList("equipment & machinery", "equipment & office")),
            new CpvCodeAndRule("411100003", Arrays.asList("water & drinking")),
            new CpvCodeAndRule("703000004", Arrays.asList("[SPACE]venue")),
            new CpvCodeAndRule("446000006", Arrays.asList("water & tank")),
            new CpvCodeAndRule("09120000", Arrays.asList("[SPACE]gas")),
            new CpvCodeAndRule("551000001", Arrays.asList("hotel services", "hotel")),
            new CpvCodeAndRule("501000006", Arrays.asList("servic & vehicle", "repair & vehicle", "repair motor", "service repair",
                    "vehicle maintenance", "repair service")),
            new CpvCodeAndRule("349800000", Arrays.asList("air ticket", "return air")),
            new CpvCodeAndRule("343200006", Arrays.asList("spare parts")),
            new CpvCodeAndRule("301900007", Arrays.asList("office equipment")),
            new CpvCodeAndRule("391300002", Arrays.asList("office furniture")),
            new CpvCodeAndRule("551200007", Arrays.asList("conference facilities", "conference")),
            new CpvCodeAndRule("301927008", Arrays.asList("stationery")),
            new CpvCodeAndRule("452000009", Arrays.asList("civil works")),
            new CpvCodeAndRule("909100009", Arrays.asList("cleaning services")),
            new CpvCodeAndRule("391413005", Arrays.asList("cabinet")),
            new CpvCodeAndRule("442111106", Arrays.asList("cabin")),
            new CpvCodeAndRule("249511006", Arrays.asList("lubricants")),
            new CpvCodeAndRule("09100000", Arrays.asList("fuel")),
            new CpvCodeAndRule("425100004", Arrays.asList("air condition")),
            new CpvCodeAndRule("398000000", Arrays.asList("cleaning material", "cleaning")),
            new CpvCodeAndRule("331000001", Arrays.asList("reagent tests")),
            new CpvCodeAndRule("301997306", Arrays.asList("business cards")),
            new CpvCodeAndRule("450000007", Arrays.asList("construction & work", "construction")),
            new CpvCodeAndRule("798100005", Arrays.asList("printing")),
            new CpvCodeAndRule("800000004", Arrays.asList("training", "workshop", "education")),
            new CpvCodeAndRule("480000008", Arrays.asList("software")),
            new CpvCodeAndRule("343500005", Arrays.asList("tyres")),
            new CpvCodeAndRule("390000002", Arrays.asList("furniture")),
            new CpvCodeAndRule("228000008", Arrays.asList("books")),
            new CpvCodeAndRule("391100006", Arrays.asList("chairs")),
            new CpvCodeAndRule("797100004", Arrays.asList("security services", "security & services")),
            new CpvCodeAndRule("301000000", Arrays.asList("printer", "toner")),
            new CpvCodeAndRule("311000007", Arrays.asList("generator")),
            new CpvCodeAndRule("150000008", Arrays.asList("food", "meals", "refreshments", "lunch", "coffee", "tea")),
            new CpvCodeAndRule("660000000", Arrays.asList("insurance")),
            new CpvCodeAndRule("330000000", Arrays.asList("laboratory", "tube", "tests", "medical")),
            new CpvCodeAndRule("301200006", Arrays.asList("photocop")),
            new CpvCodeAndRule("793400009", Arrays.asList("advertising")),
            new CpvCodeAndRule("799000003", Arrays.asList("travel")),
            new CpvCodeAndRule("310000006", Arrays.asList("electrical")),
            new CpvCodeAndRule("421200006", Arrays.asList("pump")),
            new CpvCodeAndRule("482000000", Arrays.asList("internet")),
            new CpvCodeAndRule("712500005", Arrays.asList("survey")),
            new CpvCodeAndRule("751250008", Arrays.asList("tourism")),
            new CpvCodeAndRule("09300000", Arrays.asList("solar")),
            new CpvCodeAndRule("337100000", Arrays.asList("toilet")),
            new CpvCodeAndRule("488000006", Arrays.asList("server")),
            new CpvCodeAndRule("799800007", Arrays.asList("subscription")),
            new CpvCodeAndRule("391000003", Arrays.asList("desks")),
            new CpvCodeAndRule("224000004", Arrays.asList("certificate")),
            new CpvCodeAndRule("349000006", Arrays.asList("safety")),
            new CpvCodeAndRule("302000001", Arrays.asList("digital", "comnew CpvRuleer", "media",
                    "laptop", "equipment & it[SPACE]")),
            new CpvCodeAndRule("791000005", Arrays.asList("legal")),
            new CpvCodeAndRule("799540006", Arrays.asList("party")),
            new CpvCodeAndRule("90600000", Arrays.asList("sanitation")),
            new CpvCodeAndRule("551100004", Arrays.asList("accommodation")),
            new CpvCodeAndRule("358000002", Arrays.asList("uniforms")),
            new CpvCodeAndRule("453400002", Arrays.asList("fencing")),
            new CpvCodeAndRule("441000001", Arrays.asList("plumbing", "construction & material")),
            new CpvCodeAndRule("793410006", Arrays.asList("adverts")),
            new CpvCodeAndRule("222000002", Arrays.asList("newspaper")),
            new CpvCodeAndRule("722000007", Arrays.asList("license")),
            new CpvCodeAndRule("600000008", Arrays.asList("transportation")),
            new CpvCodeAndRule("320000003", Arrays.asList("telecommunication", "communication", "radio")),
            new CpvCodeAndRule("391411003", Arrays.asList("shelves")),
            new CpvCodeAndRule("710000008", Arrays.asList("consultancy services", "study")),
            new CpvCodeAndRule("336000006", Arrays.asList("acetaminophen", "acetate", "acetazolamide", "acetic", "acetonide", "acetyl",
                    "acetylcysteine", "acetylsalicylic", "aciclovir", "actraphane", "acute", "adenosine", "adjuvant",
                    "adrenal", "adrenaline", "adriamycin", "aerosol", "aerosol-holding", "airway", "albendazole", "albumin",
                    "alginate", "alkaloids", "allis", "allopurinol", "alprazolam", "ambidextrose", "amethocaine", "drugs",
                    "amidotrizoate", "amiloride", "amino", "aminocyclitol", "aminophylline", "amiodarone", "amitriptyline",
                    "amlodipine", "amodiaquine", "amoxicillin", "amoxycillin", "amoxycillin+clavulanic", "amphotericin",
                    "ampicillin", "anaesthetic", "analgesics", "anaphylaxis", "anastrozole", "androctonus", "androgens",
                    "anhydrous", "antacids", "antenatal", "anthelmintics", "anthrax", "antibiotics", "anti-allergics",
                    "anti-bacterial", "anti-coagulants", "anti-d", "anti-haemophiliac", "anti-infective", "anti-inflammatory",
                    "anti-leprosy", "anti-metabolites", "anti-migraine", "anti-neoplastic", "anti-oxytocics", "anti-parasitic",
                    "anti-platelet", "anti-tetanus", "anti-toxins", "antiamoebic", "antianginal", "antiarrhythmic",
                    "antiasthmatic", "antibacterial", "antidotes", "antiemetic", "antiepileptic",
                    "antiepilepticsanticonvulsants", "antifilarials", "antifungal", "antigiardiasis", "antiglaucoma",
                    "antihaemorrhoids", "antihelminthics", "antihepatitis", "antiherpes", "antihormones", "antihypertensive",
                    "antiinfective", "antileishmaniasis", "antimalarial", "antimigraine", "antineoplastic", "antiparkinsonism",
                    "antipneumocystosis", "antiprotozoal", "antipruritic", "antipsychotic", "antirabies", "antiretrovirals",
                    "antischistosomals", "antiscorpion", "antiseptic", "antiseptics", "antisera", "antisnake",
                    "antispasmodics", "antistatic", "antithrombotic", "antithyroid", "antitoxoplasmosis", "antitrematode",
                    "antitrypanosomal", "antituberculosis", "antiulcer", "antivenom", "antiviral", "anxiolytics", "applicator",
                    "aqueous", "arabinosate", "armytage", "artemether", "arterial", "artery", "artesunate", "artesunic",
                    "ascorbic", "asparaginase", "aspirin", "astringents", "atazanavir", "atenolol", "atorvastatin",
                    "atracurium", "atropine", "attenuated", "autoclavable", "azathioprine", "azithromycin", "backhaus",
                    "balfour", "bandage", "barium", "beclomethasone", "bedaquiline", "bedpan", "bein", "bendrofluazide",
                    "bendroflume-", "benzathine", "benzhexol", "benzoate", "benzocaine", "benzoic", "benzoyl",
                    "benzoylperoxide", "benztropine", "benzyl", "benzylpenicillin", "besilate", "beta-blocker",
                    "beta-lactam", "betamethasone", "bicalutamide", "bicarbonate", "bicuspids", "biliary", "bioharzard",
                    "biopsy", "biphasic", "bipolar", "bisacodyl", "bismuth", "bisoprolol", "bisulfate", "bitis", "bleomycin",
                    "blister", "blood", "borditella", "braun-stadler", "bromide", "bromocriptine", "bronchial",
                    "bronchodilators", "bupivacaine", "buprenorphine", "burette", "buthus", "butylbromide", "byrant",
                    "caesarian", "calamine", "calcium", "cancellous", "cancer", "candesartan", "cannula", "capecitabine",
                    "caplan", "capreomycin", "capsid", "captopril", "carbamazepine", "carbamazine", "carbidopa", "carbimazole",
                    "carbonate", "carbonating", "carboplatin", "cardiovascular", "carmustine", "catheter", "cefalexin",
                    "cefazolin", "cefixime", "cefotaxime", "cefoxitine", "ceftriaxone", "cefuroxime", "cephalexin",
                    "cephalosporins", "cephamycins", "cerebrolysin", "cervical", "cetirizine", "cetrimide", "ch10", "ch26",
                    "cheattle", "chlorambucil", "chloramphenicol", "chlorhexidine", "chloride", "chloroquine", "chloroxylenol",
                    "chlorphenamine", "chlorpheniramine", "chlorpromazine", "cholinesterase", "chromoglicate", "chromoglycate",
                    "cilastatin", "cilexetil", "cimetidine", "ciprofloxacin", "circumcision", "cisatracurium", "cisplatin",
                    "citrate", "clavulanic", "clindamycin", "clinic", "clinician", "clofazimine", "clomifene", "clomiphene",
                    "clomipramine", "clonazepam", "clonidine", "clopidogrel", "clotrimazole", "cloxacillin", "co-trimoxazole",
                    "coagulation", "codeine", "colchicine", "colostomy", "condom", "contraceptives", "cortical",
                    "corticosteroids", "corticotrophin", "cortisone", "cotrimoxazole", "crylate", "curative", "cure",
                    "curette", "cutaneous", "cyclizine", "cyclophosphamide", "cycloserine", "cyclosporin", "cyproteron",
                    "cyproterone", "cytarabine", "cytosine", "cytotoxic", "dacarbazine", "dactinomycin", "dapsone",
                    "darcabazine", "darrows", "darunavir", "daunorubicin", "decanoate", "decongestants", "deferoxamine",
                    "denatured", "dendroaspis", "dental", "depressor", "derivative", "dermatological", "desferrioxamine",
                    "desferroxamine", "desflurane", "dexamethasone", "dextran", "dextrose", "diabetes", "diabetic",
                    "diagnosis", "diagnostic", "dialyser", "dialysers", "dialysis", "diameter", "diaslysis", "diathermy",
                    "diazepam", "dicitratobismuthate", "diclofenac", "dietary", "diethylcarbamazine", "digluconate",
                    "digoxin", "dihydrate", "dihydroartemisinin", "dilator", "dimercaprol", "dinitrate", "dinoprostone",
                    "diphosphate", "diphtheria", "dipivefrine", "diploid", "diptheria-pertussis", "disease", "diseases",
                    "disinfectants", "disodium", "disoproxil", "dissecting", "dissector", "disulfiram", "diuretics", "dmards",
                    "dmpa-im", "dmpa-sc", "dobutamine", "docetaxel", "dolutegravir", "domperidone", "dopamine", "dosage",
                    "dose", "doxorubicin", "doxycycline", "droperidol", "drotaverine", "drugs", "dynamic", "eamophilia",
                    "econazole", "edetate", "efavirenz", "eflornithine", "ejectors", "emitricitabine", "enalapril", "endocrine",
                    "endotracheal", "enoxaparin", "entecavir", "ephedrine", "epidural", "epinephrine", "epirubicin", "episiotomy",
                    "ergocalciferol", "ergometrine", "ergotamine", "erythromycin", "esmarch", "esophageal", "ethambutol", "ethanol",
                    "ethinylestradiol", "ethionamide", "ethosuximide", "etomidate", "etonogestrel", "etoposide", "eugenol",
                    "examination", "femur", "fenofibirate", "fentanyl", "fergusson-ackland", "ferrate", "ferric", "ferrous",
                    "fexofenadine", "filgastrin", "filgrastim", "fistula", "fixators", "fixer", "flanges", "flucloxacillin",
                    "fluconazole", "flucytosine", "fludarabine", "fludrocortisone", "flumazenil", "fluorescein", "fluorouracil",
                    "fluoxetine", "fluphenazine", "foerster", "foley", "folic", "folinate", "folinic", "forceps", "fore-arm",
                    "form-chest", "form-plain", "form-skull", "form-spine", "formaldehyde", "formalin", "formoterol", "formula",
                    "forscarnet", "fractiondried", "frusemide", "fumarate", "furosemide", "gabapentin", "gadolineum", "gallipot",
                    "ganciclovir", "gastrointestinal", "gauze", "gemcitabine", "gentamicin", "gentian", "gilles", "glibenclamide",
                    "gliclazide", "glipizide", "globulin", "glucagon", "gluconate", "glucose", "glutaraldehyde", "glyceryl",
                    "glycosides", "gonadal", "goserelin", "gout", "granisetron", "granules", "granulocyte", "griseofulvin",
                    "guedel", "gynaecological", "gynaecology", "gyno", "haemodialysis", "haemoglobinopathies", "haemophilus",
                    "haemopoetics", "haemostat", "haemostatic", "haemostats", "haloperidol", "halothane", "halstead-mosq",
                    "handfoot", "handscrub", "handwash", "hartmann", "hartmann'sringer'", "hbmf", "healing", "hegar",
                    "hemodialysis", "heparin", "hepatitis", "hepb-hib", "hexacyano", "hexidine", "histology", "hollow-ware",
                    "hormonal", "hormones", "hospital", "humidifier", "hyaluronate", "hyaluronidase", "hydralazine",
                    "hydrochloride", "hydrochlorideml", "hydrochlorothiazide", "hydrochlorthiazide", "hydrocortisone",
                    "hydrogen", "hydrophilic", "hydroxide", "hydroxocobalamin", "hydroxycarbamide", "hydroxyethylcellu",
                    "hydroxyurea", "hyoscine", "hyperimmune", "hyperosmatic", "hypochlorite", "hypodermic", "hypromellose",
                    "hysterectomy", "ibuprofen", "ichthammol", "ifosfamide", "ifosfamide-mesna", "ilizarov", "imatinib",
                    "imipenem", "immunisation", "immunoglobulin", "immunoglobulins", "immunological", "immunologicals",
                    "immunosuppressive", "implant", "implantable", "impregnated", "impression", "in-patient", "incisors",
                    "indinavir", "inducers", "infections", "influenza", "influenzae", "inhalation", "inhalational", "inhaler",
                    "inhibitors", "injectable", "injection", "injury", "insulin", "integrase", "interferon", "intestinal",
                    "intra-nasal", "intracameral", "intradermal", "intraocular", "intraperitoneal", "intrauterine", "iodide",
                    "iodine", "iodineml", "iodoform", "iohexol", "ionic", "ionometer", "iopamidol", "iopromide", "iotroxate",
                    "ipecacuanha", "ipratropium", "irinotecan", "iron", "iron+", "ironml", "isethionate", "isoflurane",
                    "isoniazid", "isophane", "isoprenaline", "isosorbide", "isotonic", "ispaghula", "itis", "k-wires",
                    "kanamycin", "keratolytics", "keratomes", "keratoplastics", "ketaconazole", "ketamine", "ketoconazole",
                    "ketokonazole", "kidney", "kocher", "l-asparaginase", "l-plates", "labetalol", "lactate", "lactulose",
                    "lamivudine", "lamotrigine", "langenbeck", "laryngoscope", "laryngoscopes", "latanoprost", "laxatives",
                    "ld50", "leiurus", "leprosy", "leucovorin", "levodopa", "levodopa-carbidopa", "levofloxacin",
                    "levonorgestrel", "levonorgestrel-releasing", "levothyroxine", "lidocaine", "ligatures", "lignocaine",
                    "linezolid", "lingual", "lipid-lowering", "lipid-regulating", "liposomal", "lisinopril", "lithium",
                    "lomustine", "loperamide", "lopinavir", "lopinavir/ritonavir", "losartan", "lubricants", "lugol's",
                    "lumbar", "lumefantrine", "macrolides", "magnesium", "malaria", "maleate", "malignant", "mammography",
                    "mannitol", "mayo-stille", "mcgdose", "mcindoe", "measles", "mebendazole", "medazolam", "medical",
                    "medicated", "medication", "medicine", "medicines", "medroxyprogeste", "medroxyprogesterone", "mefenamic",
                    "mefloquine", "meglumine", "melarsoprol", "melphalan", "menefegol", "meningococcal", "mercaptopurine",
                    "mesilate", "mesna", "mesylate", "metformin", "methionine", "methotrexate", "methyldopa", "methylene",
                    "methylphenidate", "methylpredni", "methylrosanilinium", "methyltestosterone", "methylthioninium",
                    "metoclopramide", "metoprolol", "metronidazole", "metzenbaum", "mhc-radiographic", "miconazole",
                    "microgram", "micron", "midazolam", "mifepristone", "miotics", "misoprostol", "mitomycin", "mitoxantrone",
                    "mldose", "monohydrate", "morphine", "mortuary", "mouth", "mouthwash", "moxifloxacin", "mucus",
                    "multivitamin", "mupirocin", "muscle", "musco-skeletal", "mustine", "mydriatics", "myometrial",
                    "n-acetylcysteine", "nalidixic", "naloxone", "naltrexone", "nasal", "nasogastric", "nasogastricfeeding",
                    "natamycin", "nebuliser", "neck", "necropsy", "needle", "needles", "nelaton", "nelfinavir", "neomycin",
                    "neomycin/betamethasone", "neonatal", "neonate", "neonates", "neostigmine", "neous", "nevirapine",
                    "niclosamide", "nicotinamide", "nicotine", "nifedipine", "nifurtimox", "nitrate", "nitrile",
                    "nitrofurantoin", "nitroprusside", "nitrous", "non-electric", "non-ionic", "non-nucleoside",
                    "non-opiod", "non-opioids", "non-specific", "non-sterile", "non-steroidal", "non-tooth", "noradrenaline",
                    "norepinephrine", "norethisterone", "norgestrel", "nose", "nsaids", "nucleoside", "nucleotide", "nurse",
                    "nursing", "nutrition", "nylon", "nystatin", "oblique", "obstetrics", "occlussal", "oestrogens", "ofloxacin",
                    "ointment", "olanzapine", "omeprazole", "ondansetron", "ophthalmic", "ophthalmological", "opioid", "oral",
                    "oralnasal", "oropharyngeal", "ortho-phthaldehyde", "orthopaedic", "orthopaedicc", "orthopaedics", "oseltamivir",
                    "osmolarity", "osmotic", "out-patient", "ovalround", "ovulation", "ovum", "oxaliplatin", "oxide", "oximeter",
                    "oxygen", "oxymetazoline", "oxytetracycline", "oxytocics", "oxytocin", "p-aminosalicylic", "paclitaxel",
                    "paediatric", "palliative", "palmitate+dl-alpha-tocopherol", "panaromic", "pancuronium", "papilloma",
                    "papiloma", "paracetamol", "paraffin", "parenteral", "parkinsonism", "paromomycin", "patients",
                    "pediculicides", "pellets", "penicillamine", "penicillin", "penile", "pentamidine", "pentavalent",
                    "peri-operative", "periapical", "periodontometer", "peripherally", "peripherally-acting", "peritoneal",
                    "permacath", "permanganate", "permethrin", "peroxide", "pertussis", "pertussis-tetanus", "pessary",
                    "pethidine", "pheniramine", "phenobarbital", "phenobarbitone", "phenoxymethyl", "phenylephrine", "phenytoin",
                    "phosphate", "physiotherapy", "physostigmine", "phytomenadione", "picc", "pilocarpine", "piperacillin",
                    "piracetam", "pivodine", "pizotifen", "plaincuffed", "plasma", "plastified", "platelets", "plates-t-plates",
                    "pmma", "pneumococcal", "pneumocystis", "pneumonia", "podophyllum", "poisoning", "poliomyelitis", "polishing",
                    "polygeline", "polyglycolic", "polymethylmetha", "polymyxin", "polypropylene", "polysaccharide", "polyureth",
                    "polyvalent", "polyvenum", "polyvidone-iodine", "postaglandin", "potassium", "povidone", "povidone-iodine",
                    "pralidoxime", "praziquantel", "prazosin", "prednisolone", "prefilled", "premature", "preoperative",
                    "prescription", "primaquine", "procainamide", "procaine", "procarbazine", "prochlorperazine", "proctoscope",
                    "procyclidine", "progesterone-only", "progestogen", "progestogens", "promethazine", "propantheline",
                    "proparacaine", "prophylaxis", "propofol", "propranolol", "prostheses", "prosthesis", "protamine",
                    "protease", "prothionamide", "psychoactive", "pulmonary", "pulse", "pumice", "purified", "pyrantel",
                    "pyrazinamide", "pyridoxine", "pyrimethamine", "quetiapine", "quinidine", "quinine", "quinolones",
                    "rabeprazole", "rabies", "radiocontrast", "radiology", "radiopaque", "raltegravir", "ramipril", "rampley",
                    "ranitidine", "rasparatory", "rectal", "regulator", "rehydration", "relaxants", "remifentanil", "resection",
                    "resin", "resperidone", "respiratory", "respiratory system", "resuscitator", "retaining", "retinol",
                    "retractor", "retractors", "rheumatic", "rheumatoid", "riboflavine", "rifabutin", "rifampicin",
                    "risperidone", "ritonavir", "rocuronium", "rotacap", "rotavirus", "rash", "rutf", "rylelevin", "salbutamol",
                    "salicylic", "saline", "saliva", "saquinavir", "scabicides", "scaler", "scalp", "scalpel", "schanz",
                    "schistosomicides", "seldin", "selegiline", "sertraline", "sevoflurane", "simvastatin", "sofosbuvir",
                    "sonography", "specialist", "spectinomycin", "speculum", "spencer-wells", "spermicide", "spinal",
                    "spironolactone", "spongedressing", "statins", "stavudine", "steinman", "sterile", "sterilisation",
                    "stibogluconate", "stilboestrol", "stille", "stimulants", "stimulating", "stitchligature", "streptokinase",
                    "streptomycin", "subcuta", "subgallate", "sublingual", "succinate", "sulfadiazine", "sulfadoxine",
                    "sulfamethoxazole", "sulfate", "sulphadiazine", "sulphadoxine", "sulphate", "sulphonamides", "sumatriptan",
                    "suramin", "surgeon", "surgeontheatre", "surgical", "suture", "suxamethonium", "swab", "sympathomimetics",
                    "syringes", "syringe", "t-plates", "t-tube", "t380a", "tamoxifen", "tartrate", "tazobactam", "teale",
                    "teeth", "temozolamide", "temperature", "tenaculum", "tenckhoff", "tennaculum", "tenofovir", "terbutaline",
                    "tered", "testosterone", "tetanus", "tetracaine", "tetracycline", "thalidomide", "therapeutic-feeds",
                    "thermometer", "thiabendazole", "thiamine", "thiazide", "thioguanine", "thiopental", "thiopentone",
                    "thioridazine", "thiosulphate", "thorax", "throat", "thyroid", "thyroxine", "tibia", "timolol",
                    "tincture", "tinidazole", "tobramycin", "tocolytics", "tocopheryl", "tooth", "tourniquet", "toxoid",
                    "tracer", "tracheostomy", "tramadol", "tranexamic", "transcriptase", "transducer", "transfer", "transfusion",
                    "triamcinolone", "trifluoperazine", "trihexyphenidyl", "trihydrate", "trimethoprim", "trinitrate",
                    "tripotassium", "trisilicate", "trisodium", "tropicamide", "trypanocides", "trypanosomiasis", "tryptan",
                    "tuberculin", "tuberculosis", "typhoid", "ulcer", "ultrasound", "umbilical", "undercast", "urea",
                    "urethral", "urinal", "urine", "uterine", "vaccine", "vaginal", "valproate", "vancomycin", "vasodilators",
                    "vecuronium", "vein", "venous", "verapamil", "vero", "vinblastine", "vincristine", "virus", "viscoelastics",
                    "vitamin", "volkmann", "vulsellum", "warfarin", "weitlaner", "wertheim", "williger", "wound", "x-ray",
                    "xylometazoline", "yellow fever", "y-line", "y-site", "zidovudine", "zinc", "zipp", "zuclopenthixol"
            ))
    );

    private static final Map<String, CityAndNuts> buyersCitiesAndNuts = new HashMap<String, CityAndNuts>(){{
        put("Ministry of East African Community Affairs", new CityAndNuts("Kampala", "UG100"));
        put("Fort Portal Municipal Council", new CityAndNuts("Fort Portal", "UG400"));
        put("National Medical Stores", new CityAndNuts("Entebbe", "UG100"));
        put("Ministry of Education, Science, Technology and Sports", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Justice and Constitutional Affairs", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Revenue Authority", new CityAndNuts("Kampala", "UG100"));
        put("Civil Aviation Authority", new CityAndNuts("Entebbe", "UG100"));
        put("Ministry of Health", new CityAndNuts("Kampala", "UG100"));
        put("Cotton Development Organization", new CityAndNuts("Kampala", "UG100"));
        put("National Social Security Fund", new CityAndNuts("Kampala", "UG100"));
        put("National Drug Authority", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Energy and Mineral Development", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Registration Services Bureau", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Coffee Development Authority", new CityAndNuts("Kampala", "UG100"));
        put("Mpigi District Local Government", new CityAndNuts("Mpigi", "UG100"));
        put("Ministry of Finance, Planning and Economic Development", new CityAndNuts("Kampala", "UG100"));
        put("Uganda National Roads Authority", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Information and Communications Technology", new CityAndNuts("Kampala", "UG100"));
        put("UGANDA POLICE FORCE", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Road Fund", new CityAndNuts("Kampala", "UG100"));
        put("Bukedea District Local Government", new CityAndNuts("Bukedea", "UG200"));
        put("Wakiso", new CityAndNuts("Kampala", "UG100"));
        put("Kampala Capital City Authority", new CityAndNuts("Kampala", "UG100"));
        put("National Water & Sewerage Corporation", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Electricity Transmission Company", new CityAndNuts("Kampala", "UG100"));
        put("Uganda National Examinations Board", new CityAndNuts("Kampala", "UG100"));
        put("National Planning Authority", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Tourism Board", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Wildlife Authority", new CityAndNuts("Kampala", "UG100"));
        put("Gulu University", new CityAndNuts("Gulu", "UG300"));
        put("LAW DEVELOPMENT CENTRE", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Broadcasting Corporation", new CityAndNuts("Kampala", "UG100"));
        put("Nwoya District Local Government", new CityAndNuts("Nwoya", "UG300"));
        put("Uganda Electricity Distribution Company", new CityAndNuts("Kampala", "UG100"));
        put("Directorate of Government Analytical Laboratory", new CityAndNuts("Kampala", "UG100"));
        put("Mbale Regional Referral Hospital", new CityAndNuts("Mbale", "UG200"));
        put("Mbale Municipality", new CityAndNuts("Mbale", "UG200"));
        put("Uganda Human Rights Commission", new CityAndNuts("Kampala", "UG100"));
        put("Rural Electrification Agency", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Gender, Labour and Social Development", new CityAndNuts("Kampala", "UG100"));
        put("Busitema University", new CityAndNuts("Busitema", "UG200"));
        put("Muni University", new CityAndNuts("Arua", "UG300"));
        put("Electoral Commission", new CityAndNuts("Kampala", "UG100"));
        put("Office of the Prime Minister", new CityAndNuts("Kampala", "UG100"));
        put("Ngora District Local Government", new CityAndNuts("Ngora District", "UG200"));
        put("Uganda Investment Authority", new CityAndNuts("Kampala", "UG100"));
        put("Diary Development Authority", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Tourism Wildlife and Antiquities", new CityAndNuts("Kampala", "UG100"));
        put("National Environment Management Authority", new CityAndNuts("Kampala", "UG100"));
        put("Entebbe Municipal Council", new CityAndNuts("Entebbe", "UG100"));
        put("Uganda Industrial Research Institute", new CityAndNuts("Kampala", "UG100"));
        put("Public Procurement and Disposal of Public Assets Authority", new CityAndNuts("Kampala", "UG100"));
        put("Uganda National Bureau of Standards", new CityAndNuts("Kampala", "UG100"));
        put("Jinja Municipal Council", new CityAndNuts("Jinja", "UG200"));
        put("Kayunga District Local Government", new CityAndNuts("Kayunga", "UG100"));
        put("Wakiso District Local Government", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Water and Environment", new CityAndNuts("Kampala", "UG100"));
        put("Hoima Municipality", new CityAndNuts("Hoima", "UG400"));
        put("National Information and Technology Authority", new CityAndNuts("Kampala", "UG100"));
        put("Uganda National Meteorological Authority", new CityAndNuts("Kampala", "UG100"));
        put("Mukono District Local Government", new CityAndNuts("Kampala", "UG100"));
        put("UGANDA PRISONS SERVICE", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Lands, Housing and Urban Development", new CityAndNuts("Kampala", "UG100"));
        put("Iganga Municipal Council", new CityAndNuts("Iganga", "UG200"));
        put("National Agricultural Advisory Services", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Agriculture, Animal Industry and Fisheries", new CityAndNuts("Entebbe", "UG100"));
        put("Local Government Finance Commission", new CityAndNuts("Kampala", "UG100"));
        put("Kabarole District Local Government", new CityAndNuts("Fort Portal", "UG400"));
        put("Uganda AIDS Commission", new CityAndNuts("Kampala", "UG100"));
        put("INSPECTORATE OF GOVERNMENT", new CityAndNuts("Kampala", "UG100"));
        put("Health Service Commission", new CityAndNuts("Kampala", "UG100"));
        put("Bank of Uganda", new CityAndNuts("Kampala", "UG100"));
        put("Mulago Hospital Complex", new CityAndNuts("Kampala", "UG100"));
        put("External Security Organization", new CityAndNuts("Kampala", "UG100"));
        put("Kisoro District Local Government", new CityAndNuts("Kisoro", "UG400"));
        put("National Forestry Authority", new CityAndNuts("Kampala", "UG100"));
        put("Makerere University", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Heart Institute", new CityAndNuts("Kampala", "UG100"));
        put("Mbale District Local Government", new CityAndNuts("Mbale", "UG200"));
        put("Gulu Municipal Council", new CityAndNuts("Gulu", "UG300"));
        put("Kabale District Local Government", new CityAndNuts("Kabale", "UG400"));
        put("Capital Markets Authority", new CityAndNuts("Kampala", "UG100"));
        put("Parliament of Uganda", new CityAndNuts("Kampala", "UG100"));
        put("FINMAP III", new CityAndNuts("Kampala", "UG100"));
        put("Electricity Regulatory Authority", new CityAndNuts("Kampala", "UG100"));
        put("Soroti District Local Government", new CityAndNuts("Soroti", "UG200"));
        put("Ministry of Works and Transport", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Bureau of Statistics", new CityAndNuts("Kampala", "UG100"));
        put("Adjumani District Local Government", new CityAndNuts("Adjumani", "UG300"));
        put("Arua Municipal Council", new CityAndNuts("Arua", "UG300"));
        put("Kiboga District Local Government", new CityAndNuts("Arua", "UG300"));
        put("Masindi District Local Government", new CityAndNuts("Masindi", "UG400"));
        put("Pallisa District Local Government", new CityAndNuts("Pallisa", "UG200"));
        put("Mubende District Local Government", new CityAndNuts("Mubende", "UG100"));
        put("Mityana District Local Government", new CityAndNuts("Mityana", "UG100"));
        put("Directorate of Public Prosecutions", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Internal Affairs", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Property Holdings", new CityAndNuts("Kampala", "UG100"));
        put("Lira District Local Government", new CityAndNuts("Lira", "UG300"));
        put("National Agricultural Research Organization", new CityAndNuts("Entebbe", "UG100"));
        put("Soroti Municipality", new CityAndNuts("Soroti", "UG200"));
        put("Ministry of Public Service", new CityAndNuts("Kampala", "UG100"));
        put("Judicial Service Commission", new CityAndNuts("Kampala", "UG100"));
        put("Arua District Local Government", new CityAndNuts("Arua", "UG300"));
        put("Pride Microfinance Limited", new CityAndNuts("Kampala", "UG100"));
        put("Busia Municipal Council", new CityAndNuts("Busia", "UG200"));
        put("Uganda Development Bank", new CityAndNuts("Kampala", "UG100"));
        put("Mbarara University Of Science and Technology", new CityAndNuts("Mbarara", "UG400"));
        put("Uganda Exports Promotions Board", new CityAndNuts("Kampala", "UG100"));
        put("Kumi District Local Government", new CityAndNuts("Kumi", "UG200"));
        put("Ministry of Local Government", new CityAndNuts("Kampala", "UG100"));
        put("Masaka Referral Hospital", new CityAndNuts("Masaka", "UG100"));
        put("Insurance Regulatory Authority", new CityAndNuts("Kampala", "UG100"));
        put("The Microfinance Support Centre Ltd", new CityAndNuts("Kampala", "UG100"));
        put("Napak District Local Government", new CityAndNuts("Napak", "UG300"));
        put("Uganda Land Commission", new CityAndNuts("Kampala", "UG100"));
        put("Mbarara District Local Government", new CityAndNuts("Mbarara", "UG400"));
        put("Atomic Energy Council", new CityAndNuts("Kampala", "UG100"));
        put("Lira University", new CityAndNuts("Lira", "UG300"));
        put("Kapchorwa District Local Government", new CityAndNuts("Kapchorwa", "UG200"));
        put("JINJA DISTRICT LOCALGOVERNMENT", new CityAndNuts("Jinja", "UG200"));
        put("Maracha District Local Government", new CityAndNuts("Maracha", "UG300"));
        put("Lira Municipal Council", new CityAndNuts("Lira", "UG300"));
        put("Uganda Communications Commission", new CityAndNuts("Kampala", "UG100"));
        put("Directorate of Citzenship and Immigration Control", new CityAndNuts("Kampala", "UG100"));
        put("Amnesty Commission", new CityAndNuts("Kampala", "UG100"));
        put("Education Service Commission", new CityAndNuts("Kampala", "UG100"));
        put("Public Service commission", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Foreign Affairs", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Defence", new CityAndNuts("Kampala", "UG100"));
        put("Office of the Auditor General", new CityAndNuts("Kampala", "UG100"));
        put("Soroti Regional Referral Hospital", new CityAndNuts("Soroti", "UG200"));
        put("Hoima Referral Hospital", new CityAndNuts("Hoima", "UG400"));
        put("Makerere University Business School", new CityAndNuts("Kampala", "UG100"));
        put("National Identification & Registration Authority", new CityAndNuts("Kampala", "UG100"));
        put("Directorate of Ethics and Integrity", new CityAndNuts("Kampala", "UG100"));
        put("National Council for Higher Education", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Law Reform Commission", new CityAndNuts("Kampala", "UG100"));
        put("Courts of Judicature", new CityAndNuts("Kampala", "UG100"));
        put("Competitiveness and Enterprise Development Project", new CityAndNuts("Kampala", "UG100"));
        put("Higher Education Students' Financing Board", new CityAndNuts("Kampala", "UG100"));
        put("Bushenyi Ishaka Municpal council", new CityAndNuts("Bushenyi", "UG400"));
        put("Masaka Municipality", new CityAndNuts("Masaka", "UG100"));
        put("Ministry of Trade, Industry and Co-operative", new CityAndNuts("Kampala", "UG100"));
        put("Uganda Wildlife Education Centre", new CityAndNuts("Entebbe", "UG100"));
        put("Sironko District Local Government", new CityAndNuts("Sironko", "UG200"));
        put("Uganda Electricity Generation Company", new CityAndNuts("Kampala", "UG100"));
        put("Moroto Municipal Council", new CityAndNuts("Moroto", "UG300"));
        put("Kiryandongo District Local Government", new CityAndNuts("Kigumba", "UG400"));
        put("Iganga District Local Government", new CityAndNuts("Iganga", "UG200"));
        put("Kabale Municipal Council", new CityAndNuts("Kabale", "UG400"));
        put("Arua Hospital", new CityAndNuts("Arua", "UG300"));
        put("Kyambogo University", new CityAndNuts("Kampala", "UG100"));
        put("Jinja Regional Referral Hospital", new CityAndNuts("Jinja", "UG200"));
        put("Uganda Railway Cooperation", new CityAndNuts("Kampala", "UG100"));
        put("Isingiro District Local Government", new CityAndNuts("Isingiro", "UG400"));
        put("National Animal Generic Resource Centre And Data Bank", new CityAndNuts("Entebbe", "UG100"));
        put("SheemaMunicipal Council", new CityAndNuts("Sheema", "UG400"));
        put("Kyenjojo District Local Government", new CityAndNuts("Kyenjojo", "UG400"));
        put("Tororo District Local Government", new CityAndNuts("Tororo", "UG200"));
        put("Kabale Regional Referral Hospital", new CityAndNuts("Kabale", "UG400"));
        put("Gulu District Local Government", new CityAndNuts("Gulu", "UG300"));
        put("Rubirizi District Local Government", new CityAndNuts("Rubirizi", "UG400"));
        put("Kisoro Municipal Council", new CityAndNuts("Kisoro", "UG400"));
        put("Uganda Cancer Institute", new CityAndNuts("Kampala", "UG100"));
        put("Uganda National Cultural Centre", new CityAndNuts("Kampala", "UG100"));
        put("Kamuli District Local Government", new CityAndNuts("Kamuli", "UG200"));
        put("Equal Opportunities Commmision", new CityAndNuts("Kampala", "UG100"));
        put("Nebbi District Local Government", new CityAndNuts("Nebbi", "UG300"));
        put("Fort Portal Regional Referral Hospital", new CityAndNuts("Fort Portal", "UG400"));
        put("Butabika Hospital", new CityAndNuts("Kampala", "UG100"));
        put("Ministry of Science Technology and innovation", new CityAndNuts("Kampala", "UG100"));
        put("Masaka District Local Government", new CityAndNuts("Masaka", "UG100"));
        put("Kakumiro District Local Government", new CityAndNuts("Kakumiro", "UG400"));
        put("Busia District Local Government", new CityAndNuts("Busia", "UG200"));
        put("Kumi Municipal Council", new CityAndNuts("Kumi", "UG200"));
        put("Masindi Municipality", new CityAndNuts("Masindi", "UG400"));
        put("Nebbi Municipal Council", new CityAndNuts("Nebbi", "UG300"));
        put("Ntungamo MC", new CityAndNuts("Ntungamo", "UG400"));
        put("Sheema Municipal Council", new CityAndNuts("Sheema", "UG400"));
        put("Privatization Unit", new CityAndNuts("Kampala", "UG100"));
        put("Post Bank", new CityAndNuts("Kampala", "UG100"));
        put("FINANCIAL INTELLIGENCE AUTHORITY", new CityAndNuts("Kampala", "UG100"));
        put("Mityana Municipal Council", new CityAndNuts("Mityana", "UG100"));
        put("Mubende Regional Referral Hospital", new CityAndNuts("Mubende", "UG100"));
        put("Koboko District Local Government", new CityAndNuts("Koboko", "UG300"));
        put("NEW VISION PRINTING & PUBLISHING", new CityAndNuts("Kampala", "UG100"));
        put("Lira Regional Referral Hospital", new CityAndNuts("Lira", "UG300"));
        put("Ibanda Municipal Council", new CityAndNuts("Ibanda", "UG400"));
        put("Amudat", new CityAndNuts("Amudat", "UG300"));
        put("Uganda Printing & Publishing Corporation", new CityAndNuts("Entebbe", "UG100"));
        put("Uganda Retirement Benefits Regulatory Authority", new CityAndNuts("Kampala", "UG100"));
        put("Moroto", new CityAndNuts("Moroto", "UG300"));
        put("Hoima District Local Government", new CityAndNuts("Hoima", "UG400"));
        put("Ibanda District Local Government", new CityAndNuts("Ibanda", "UG400"));
        put("Kiruhura District Local Government", new CityAndNuts("Kiruhura", "UG400"));
        put("Kabale University", new CityAndNuts("Kabale", "UG400"));
        put("Kyotera District Local Goverment", new CityAndNuts("Kyotera", "UG100"));
        put("Mulago School of Nursing and Midwifery", new CityAndNuts("Kampala", "UG100"));
        put("Koboko Munincipal Council", new CityAndNuts("Koboko", "UG300"));
        put("Soroti University", new CityAndNuts("Soroti", "UG200"));
        put("UGANDA NATIONAL COUNCIL OF SCIENCE & TECHNOLOGY", new CityAndNuts("Kampala", "UG100"));
        put("Pader District Local Government", new CityAndNuts("Pader", "UG300"));
        put("Kole 10.", new CityAndNuts("Kole", "UG100"));
        put("Mukono Municipal Council", new CityAndNuts("Mukono", "UG100"));
        put("Bushenyi District Local Government", new CityAndNuts("Bushenyi", "UG400"));
        put("JINJA DISTRICT LOCAL GOVERNMENT", new CityAndNuts("Jinja", "UG200"));
        put("Ntugamo MC", new CityAndNuts("Ntungamo", "UG400"));
        put("Nakaseke District Local Government", new CityAndNuts("Nakaseke", "UG100"));
        put("Ntungamo District Local Government", new CityAndNuts("Ntungamo", "UG400"));
        put("Rukungiri District Local Government", new CityAndNuts("Rukungiri", "UG400"));
        put("Agago District Local Government", new CityAndNuts("Agago", "UG300"));
        put("Rukungiri Municipal Council", new CityAndNuts("Rukungiri", "UG400"));
        put("Management Training and Advisory Centre", new CityAndNuts("Nakawa", "UG100"));
        put("Apac District Local Government", new CityAndNuts("Apac", "UG300"));
        put("Buikwe District Local Government", new CityAndNuts("Buikwe", "UG100"));
        put("Kyegegwa District Local Government", new CityAndNuts("Kyegegwa", "UG400"));
        put("Kibaale Distriict Local Government", new CityAndNuts("Kibaale", "UG400"));
        put("Lugazi Municipal Council", new CityAndNuts("Lugazi", "UG100"));
        put("Njeru Municipality", new CityAndNuts("Njeru", "UG100"));
    }};

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
        List<CityAndNuts> citiesAndNuts = getBuyersCitiesAndNutsByName(cleanTender.getBuyers());
        if(citiesAndNuts != null) {
            for(int i = 0; i < cleanTender.getBuyers().size(); i++) {
                if(citiesAndNuts.get(i) != null) {
                    Address address = cleanTender.getBuyers().get(i).getAddress();
                    if(address == null) {
                        address = new Address();
                    }
                    ArrayList<String> nuts = new ArrayList<>();
                    nuts.add(citiesAndNuts.get(i).getNuts());
                    cleanTender.getBuyers().get(i)
                            .setAddress(address
                                    .setNuts(nuts)
                                    .setCity(citiesAndNuts.get(i).getCity()));
                }
            }
        }
        cleanTender.setCpvs(parseCpv(cleanTender.getTitle()));
        return cleanTender;
    }

    /**
     * Returns city and nuts depending on buyer name.
     * @param buyers list of buyers
     * @return list of cities and nuts for each buyer
     */
    private List<CityAndNuts> getBuyersCitiesAndNutsByName(final List<CleanBody> buyers) {
        if(buyers == null) {
            return null;
        }

        List<CityAndNuts> result = new ArrayList<>();

        for(CleanBody buyer: buyers) {
            String name = buyer.getName();
            if(name == null || name.isEmpty() || !buyersCitiesAndNuts.containsKey(name)) {
                result.add(null);
            } else {
                result.add(buyersCitiesAndNuts.get(name));
            }
        }
        return result;
    }

    /**
     * Checks if csv code fits given title by particular rule.
     * @param title title to check if code fits rule
     * @param rule rule - description of title, which should be completed if csv code fits
     * @return true if fits, false otherwise
     */
    private boolean cpvFitRule(final String title, final String rule) {

        String titleLower = title.toLowerCase();
        if(!rule.contains("&")) {
            return titleLower.contains(rule.replace("[SPACE]", " "));
        }
        String[] parts = rule.replace(" ", "").split("&");
        for(String part: parts) {
            if(!titleLower.contains(part.replace("[SPACE]", " "))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns csv depending on title.
     * @param title tender title
     * @return csv
     */
    private List<CPV> parseCpv(final String title) {
        List<CPV> cpvs = new ArrayList<>();
        cpvs.add(new CPV().setIsMain(true));
        if(title != null && !title.isEmpty()) {
            for(CpvCodeAndRule codeAndRule: CpvRules) {
                if (codeAndRule.getRule().stream().filter(Objects::nonNull).parallel().anyMatch(rule -> cpvFitRule(title,
                        rule))) {
                    cpvs.get(0).setCode(codeAndRule.getCode());
                    break;
                }
            }
        }
        if(cpvs.get(0).getCode() == null) {
            // other
            cpvs.get(0).setCode("99000000");
        }
        return cpvs;
    }
}
