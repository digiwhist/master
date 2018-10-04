package eu.dl.core.config;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic configuration class of the project. Technically singleton allowing easy
 * access to properties file. Reads configuration data just once during
 * initialization and provides access later on via public method.
 *
 * @author Kuba Krafka
 */
public enum Config {
    /**
     * Instance holder.
     */
    INSTANCE;

    private Properties properties;

    private List<String> configFiles;
    
    private static final String BASE_CONFIG_FILE = "base.properties";
    
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());
    
    /**
     * Private constructor for singleton pattern.
     */
    Config() {
    }

    /**
     * Provides instance of Config class(singleton pattern).
     *
     * @return initiated instance of Config
     */
    public static Config getInstance() {
        return INSTANCE;
    }

    /**
     * Returns value from configuration assigned to paramName.
     *
     * @param paramName the param name should be provided in "dotted" format -
     * ie "mongo.user"
     * @return value from configuration
     */
    public String getParam(final String paramName) {
        if (properties == null) {
            try {
                loadProperties();
            } catch (Exception e) {
                throw new RuntimeException("Failure during initiation of config system:", e);
            }
        }

        return properties.getProperty(paramName);
    }

    /**
     * Returns list from configuration assigned to {@code paramName}. The method assumes that value of the parameter
     * is a string which includes values separated with {@code delimiter}. From each individual value the leading and trailing
     * spaces are removed.
     *
     * @param <T>
     *      required collection class
     * @param paramName
     *      that param name
     * @param delimiter
     *      values delimiter
     * @param collectionClass
     *      required class of the resulting collection
     * @return collection of values in case that property exists, otherwise empty collection
     */
    public <T extends Collection> T getParamValueAsList(final String paramName, final String delimiter,
        final Class<T> collectionClass) {
        
        String value = getParam(paramName);

        try {
            T list = collectionClass.newInstance();
            
            if (value != null) {
                list.addAll(Arrays.asList(value.split(delimiter)).stream()
                    .map(n -> n.trim()).collect(Collectors.toList()));
            }

            return list;
        } catch (IllegalAccessException | InstantiationException e) {
            throw new RuntimeException("Failure during parsing param as collection:", e);
        }
    }

    /**
     * Returns environment id - for example "production" or "staging".
     *
     * @return value from configuration
     */
    public String getEnvId() {
        return getParam("project.name") + "_" + getParam("project.version") + "_" + getParam("env.id");
    }
    
    /**
     * Sets the source configuration file. This method supposes {@code name} without '.properties' extension.
     *
     * @param fileNames
     *      name of configuration file.
     */
    public void setConfigFile(final List<String> fileNames) {
        if (fileNames != null) {
            if (configFiles == null) {
                configFiles = new ArrayList<>();
            }

            configFiles.addAll(fileNames);
        }
    }

    /**
     * Add file to the list of processed configuration files.
     *
     * @param file
     *      name of configuration file
     */
    public void addConfigFile(final String file) {
        if (file != null) {
            if (configFiles == null) {
                configFiles = new ArrayList<>();
            }

            if (!configFiles.contains(file)) {
                configFiles.add(file);
             
                try {
                    loadProperties();
                } catch (Exception e) {
                    throw new RuntimeException("Failure during initiation of config system:", e);
                }
            }
        }
    }

    /**
     * Loads properties from the configuration file. Also attempts to find config property 'additionalPropertyFiles'
     * among loaded properties and in case that the property exist, appends each config file from this list.
     *
     * @see Config#setConfigFile(List<String>)
     *
     * @throws IOException
     *      in case that config file isn't set or properties loading from this file fails
     */
    private void loadProperties() throws IOException {
        if (configFiles == null) {
            throw new IOException("Config files aren't set");
        }

        properties = new Properties();

        // load base - shared properties file from classpath
        InputStream baseInputStream = getClass().getClassLoader().getResourceAsStream(BASE_CONFIG_FILE);
        
        if (baseInputStream != null) {
            properties.load(baseInputStream);
            baseInputStream.close();
        } else {
            logger.info("Property file '{}' not found in the classpath", BASE_CONFIG_FILE);
        }
        
        // load properties files from classpath, for each file is necessary to append extension ".properties"
        loadConfigurationFiles(configFiles.stream().map(n -> n + ".properties").collect(Collectors.toList()));
    }

    /**
     * Loads properties from the given list of configuration files.
     *
     * @param files
     *      list of configuration files
     * @throws IOException
     *      in case that configuration file isn't set or properties loading from this file fails
     */
    private void loadConfigurationFiles(final List<String> files) throws IOException {
        for (String f : files) {
		    // load properties file from classpath
		    InputStream inputStream = getClass().getClassLoader().getResourceAsStream(f);

		    if (inputStream != null) {
		        properties.load(inputStream);
		    } else {
		        throw new FileNotFoundException("Property file '" + f + "' not found in the classpath");
		    }

		    inputStream.close();
        }
    }
}
