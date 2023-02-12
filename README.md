
# T-hoarder_R

Un conjunto de notebooks en R para extraer datos de Twitter y visualizarlos

## Motivación

Este repositorio se ha generado para hacer llegar a los investigadores unas herramientas que les permitan analizar información de **Twitter** sin que tengan que tener conocimientos de programación. Estas herramientas están en la línea de t-hoarder y de t-hoarder_kit, pero añadiéndoles una capa de visualización.

Este conjunto de herramientas están programadas en R, en formato **notebook**, que combina código R con texto enriquecido (Markdown). Esto permite una documentación más legible de los pasos a seguir. Se pueden ejecutar desde **RStudio** que es una aplicación de escritorio disponible para Windows, linux y Mac. Están pensados para que se ejecuten de una vez (opción run all) pero pueden ejecutarse paso a paso. Se aconseja ejecutarlos en Rstudio en modo **visual** (pestaña de la ventana de código) para que sea más legible.

El paso de parámetros se realiza en la primera casilla del cuaderno. Podría haber creado una aplicación interactiva con Shiny pero implicaría una configuración más compleja de las herramientas. En este momento me ha parecido lo más razonable y al alcance de todo el mundo organizarlo en **notebook** con la esperanza de que usándose, se despierte la curiosidad por R y algunos se animen a hacer sus pinitos.

## Entorno de trabajo

Estos **notebooks** trabajan con esta estructura de directorios prefijada.

    dir_raiz ----+-----data      # Se guardan los datos, cada dataset en un directorio independiente
                 |
                 +-----keys      # se guardan los ficheros con las claves de acceso. 
                 |
                 +-----notebooks # Se guardan los notebooh en R
          

Al descargar los datos de github con , se descargará esta estructura. Si se opta por otra forma de organizar los datos, los notebooks tendrán que ser modificados.

## Requisitos 

Twitter ofrece desde el inicio de su creación un conjunto de APIs (Aplication Programming Interface) que permiten acceder mediante programas a sus datos. Con el tiempo han ido apareciendo nuevas APIs según las necesidades de acceso a la información, algunas gratis y otras de pago. También han ido evolucionando las versiones. Actualmente conviven la V1.1 y la V2.

Las APIs gratuitas que se usan para descargar datos son la API Standard (V1.1) y la API Académica (V2). Para la primera no se requiere ningún permiso, pero para la segunda hay que solicitar el acceso y está reservada a investigadores académicos.

Para poder utilizar estos notebooks es preciso disponer de un perfil en Twitter y, una app, con las que se generarán la credenciales de acceso a las APIs de Twitter.

Para evitar tener que especificar en cada notebook cuales son las claves de acceso, se configuran una vez, antes de usar los notebooks de descarga de datos de Twitter.

![Ciclo Análisis](https://github.com/congosto/congosto.github.io/raw/master/ciclos_t-hoarder_R.JPG)

### Configurar las credenciales de acceso a la API Standard 

A las APIs se accede mediante un protocolo OAuth (**O**pen **Auth**orization). Se trata de un protocolo para permitir la autorización de un servicio a otro sin compartir las credenciales de usuario reales, como un nombre de usuario y contraseña.

Para acceder a las APIs es necesario crear una **app** de la que se obtendrán las claves **consumer_key**, **consumer_secret** y **Bearer Token**. Con estas claves existen dos métodos de acceso:

-   **Modo aplicación**: Solo hay que usar la clave **Bearer Token**. Con esta opción solo se pueden hacer consultas para descargar tweets.
-   **Modo usuario**: Es necesario crear un token de usuario con las claves de la app **consumer_key** y **consumer_secret**. Cuando el usuario da permiso a la aplicación, se generarán las credenciales **access_token** y **access_secret**

Desde la creación de la **API V2** ya no es posible crear aplicaciones nuevas, por lo tanto hay que utilizar una app ya creada. Con este ejemplo se pone a disposición una app y un programa en python para crear un token con un usuario.

El método de acceso a las APIs en este ejemplo será **Modo usuario**

#### Crear credenciales

Para obtener las credenciales que nos permitan trabajar en **Modo usuario** usaremos el script [python make_token_Twitter.ipynb](https://github.com/congosto/token_API_V1.1/blob/main/make_token_Twitter.ipynb) disponible en github. Este script se puede ejecutar en el [entorno colab de google](https://colab.research.google.com/).

![guía para crear credenciales](https://github.com/congosto/congosto.github.io/raw/master/crear_credenciales.jpg)

Una vez obtenidas las claves se almacenarán en el **directorio keys** y se ejecutará el **notebook cfg_API_standard.Rmd**. Esto solo habrá que hacerlo una vez. Las credenciales por defecto se guardarán en el directorio keys y serán usadas en adelante por los notebooks de descarga de datos de Twitter.

### Credenciales de acceso a la API Académica 

Cuando Twitter concede el acceso a la API Académica proporciona una **app**. Acedemos a ella desde el [Developer portal](https://developer.twitter.com/). En el caso que no la hubiera creado, la crearemos desde este portal.

Una vez creada, deberemos tener algo parecido a esto:

![Developer portal](https://github.com/congosto/congosto.github.io/raw/master/developer_portal.jpg).

El acceso a las claves de la app se realiza desde el developer portal, pulsando en la llave.

![baeren token](https://github.com/congosto/congosto.github.io/raw/master/keys_academicas.jpg)

La clave que vamos a usar es **bearer token**. La primera vez el valor es visible, lo copiaremos y lo pegaremos en un fichero de texto plano cuidando que no haya espacios ni tabulaciones. Es mejor usar la opción de copia que ofrece el portal.

-   En Windows podemos usar el editor **bloc de notas**. **¡¡Atención!!** comprobar que están visibles las extensiones de los ficheros (por defecto las esconde) [como hacer visibles las extensiones de los ficheros](https://support.microsoft.com/es-es/windows/extensiones-de-nombre-de-archivo-comunes-en-windows-da4a4430-8e76-89c5-59f7-1cdbbc75cb01)
-   En Mac **TextEdit**. Escribir un fichero de texto plano sin formato con la extensión .txt no es trivial. [aquí una manera de hacerlo](https://www.cristianmonroy.com/2019/05/aprende-a-crear-archivos-txt-en-mac.html)

Si por algún error no se copiara la clave, se puede regenerar y volver a repetir el proceso de copia y pega en el editor de texto plano.

Se recomienda que estos ficheros tengan el prefijo "key", el nombre de la app y la extensión "txt". Deben ser guardados en el **directorio keys**,

A continuación se ejecutará el **notebook cfg_API_academic.Rmd**. Esto solo habrá que hacerlo una vez. Las credenciales por defecto se guardarán en el directorio keys y serán usadas en adelante por los notebooks de descarga de datos de Twitter.

## Descripción de los notebooks 

Estos **notebook** permiten la descarga de datos de Twitter y su visualización.

Se recomienda ejecutar lo cuadernos en Rstudio en modo Visual para que sean más legibles y tengamos un índice de los chunks ![modo visual](https://github.com/congosto/congosto.github.io/raw/master/modo_visual.png)

Una vez obtenidos los datos a con los notebooks de descarga, para llevar a cabo la visualización se propone dos ciclos:

-   **Ciclo simplificado**: los datos se pueden visualizar directamente. Es una forma muy rápida conocer la estructura del dataset, aunque no se podrán generar todas las gráficas por falta de datos.

-   **Ciclo completo** : se procederá a su análisis de red con la herramienta Gephi, que entre otras funciones permite la clasificación de usuarios según sus conexiones. Esta clasificación se puede incorporar a los tweets, permitiendo generar todas las gráficas.

### Ciclo simplificado

Es sencillo y rápido. En solo dos pasos podemos averiguar aspectos importantes de la propagación

![Ciclo Análisis simplificado](https://github.com/congosto/congosto.github.io/raw/master/ciclo_basico.JPG)

-   Fase 1: notebooks de descarga de tweets

    -   search_tweets_API_standard.Rmd para las búsquedas de tweets con la **API Standard** (V1.1)
    -   user_tweets_API_standard.Rmd para las búsquedas de tweets de un usuario en concreto con la **API Standard** (V1.1)
    -   search_tweets_API_academic.Rmd para las búsquedas de tweets con la **API Académica** (V2)

-   Fase 2: Notebooks de visualización

    -   spread_tweets.Rmd para visualizar propagación de tweets
    -   profiling_user.Rmd para visualizar perfiles de usuarios

### Ciclo completo:

Es más elaborado pero permite un análisis en profundidad de la propagación al tener en cuenta los datos del análisis de red.

![Ciclo Análisis completo](https://github.com/congosto/congosto.github.io/raw/master/ciclo_ARS.JPG)

-   Fase 1: notebooks de descarga de tweets

    -   search_tweets_API_standard.Rmd para las búsquedas de tweets con la **API Standard** (V1.1)
    -   search_tweets_API_academic.Rmd para las búsquedas de tweets con la **API Académica** (V2)

-   Fase 2: notebook de generación de un fichero gdf para gephi

    -   csv2gdf.Rmd obtiene de los datos descargados un fichero gdf que describe los nodos (perfiles de usuario) y las conexiones por RTs

-   Fase 2: Análisis de red en Gephi, con cálculo de la modularidad. Se exportarán de los datos de los nodos a un fichero csv

-   Fase 3: notebook para la incorporación de la clasificación de usuarios de gephi a los tweets

    -   classify_tweets.Rmd clasifica los tweets en función de la clasificación de usuarios de Gephi

-   Fase 4: Notebooks de visualización

    -   spread_tweets.Rmd para visualizar propagación de tweets (con el parámetro ARS = TRUE)

### Funciones

Se incluyen un conjunto de ficharos en R con las funciones compartidas por los notebooks. Las funciones permiten que no haya código duplicado y que los cuadernos sean más legibles. Estas son las funciones:

-   share_functions.R contiene unas funciones básicas utilizadas por todos los notebooks
-   share_functions_API_academic.R contiene las funciones específicas para la descarga de la API académica
-   share_functions_API_standard.R contiene las funciones específicas para la descarga de la API standard
-   share_functions_viz.R contiene unas funciones básicas para visualización
-   share_functions_spread_tweets.R contiene las funciones específicas para la visualización de propagación
-   share_functions_profiling_user.R contiene las funciones específicas para la visualización de perfiles

### Visualizaciones disponibles

Existen dos conjuntos de visualizaciones:

-   Visualizaciones de propagación
-   Visualizaciones de perfilado de usuarios

En ambos casos se ofrece la opción de poder hacer zoom en las gráficas especificando un rango de fechas

#### Visualizaciones de propagación

-   **Summary**: Diagrama de barras con los distintos tipos de tweets
-   **Tweets vs. reach**: una gráfica de doble escala para representar la proporción entre los tweets y el alcance (reach). Adicionalmente, etiqueta a los perfiles que se definen como influencers con el parámetro **min_followers_influencers**
-   **Tweets vs. Rts**: una gráfica de doble escala que representa la proporción entre tweets y retweets
-   **Tweets by lang**: un line chart con la propagación según los idiomas utilizados
-   **Tweets by Hashtags**: columnas apiladas con la propagación según los hashtags encontrados con más frecuencia
-   **cumulative site mentions**: Propagación temporal de los sitios web más referenciados
-   **Tweets by comumunity** (solo en ciclo completo): columnas apiladas con la propagación por comunidades
-   **Locations by group** (solo en ciclo completo): nube de palabras de la localización declarada de los autores de los tweets por comunidad
-   **Most frequent words by group** (solo en ciclo completo): nube de palabras de los textos de los tweets por comunidad

#### Visualizaciones de perfilado de usuarios

-   **Daily routine**: un scatter plot con la publicación diaria por horas
-   **Daily activity**: una chart line segregado por tipo de tweet (original, RT, reply o quote)
-   **Impact**: line chart de doble escala de los tweets publicados vs
    -   Fav
    -   RTs
    -   Replies
    -   Quotes
    -   Impresions
-   **Endgadgement**: Line chart con la evolución de las impresiones
-   **Word cloud**
    -   Most frequent words
    -   Most frequent retweets

## Ejemplos de búsquedas en Twitter 

### Tipos de consultas 

Las consultas no son exactamente igual en la API V1,1 que en la API V2, la primera dejó de actualizarse cuando se liberó la segunda que es donde se incluyen todas las novedades. Con todo, en la V1,1. se pueden hacer consultas avanzadas.

-   [búsquedas API standard V1.1](https://developer.twitter.com/en/docs/twitter-api/v1/rules-and-filtering/search-operators)
-   [búsqueda avanzada en API standard V1.1](https://www.tweetbinder.com/blog/twitter-advanced-search/)
-   [Búsquedas en API V2](https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/build-a-query)

### Descargas de un trending topic

Al ser datos recientes se pueden obtener tanto con la API standard V1.1, con el cuaderno **search_tweets_API_standard.Rmd** o con la API académica con el cuaderno **search_tweets_API_academic.Rmd**. La query será el TT.

### Descarga un evento de larga duración

(más antiguo de 10 días)

En este caso se utilizará la API Académica V2, con el cuaderno **search_tweets_API_academic.Rmd**. Conviene probar que la query es adecuada y no proporciona falsos positivos.

### Perfilar a una cuenta de usuario

Puede hacerse de dos formas:

-   Con la API standard V1.1 **users_tweets_API_standard.Rmd** que obtendrá los últimos 3.200 tweets publicados por el usuario
-   Con la API API Académica V2 **search_tweets_API_academic.Rmd** con la query "from:usuario", que obtendrá todos los tweets del usuario en el rango de fechas que se especifique

## Ejemplos de Visualizaciones 

### De propagación de tweets

Ejemplo de los tweets y el impacto de la Embajada rusa

query -\> from:@EmbajadaRusaES OR to:

#### Summary

![Ejemplo de Summary del dataset](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_summary.png)

#### Tweets vs. reach

![Ejemplo de Tweets vs. reach](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_tweets_vs_reach_zoom.png)

#### Tweets vs. Rts

![Ejemplo de Tweets vs. Rts](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_tweets_vs_RT_total.png)

#### Tweets by lang

![Ejemplo de Tweets vs. lang](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_tweets_vs_lang_total.png)

#### Tweets by Hashtags

![Ejemplo de Tweets vs. Hashtags](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_tweets_vs_hashtags_zoom.png)

#### cumulative site mentions

![Ejemplo de Propagación de los Sitios web](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_cumulative_sites.png)

#### Tweets by comumunity

![Ejemplo de Propagación de los Sitios web](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_tweets_vs_group_zoom.png)

#### Locations by group

![Ejemplo de Origen de los tweets](https://github.com/congosto/congosto.github.io/raw/master/embajada_rusa_locations_by_group.png)

#### Most frequent words by group

[Ejemplo de Origen de los tweets]<https://github.com/congosto/congosto.github.io/blob/master/embajada_rusa_words_frequency_by_group.png>)

### De perfilado de usuarios

Ejemplo con el perfil de Elon Musk

query -\> from:elommusk

#### Daily routine

![Ejemplo de Daily routine](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_daily_routine_zoom.png)

#### Daily activity

![Ejemplo de Daily activity](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_daily_activity_zoom.png)

#### tweets vs. Rts

![Ejemplo de tweets vs. Rts](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_tweets_vs_RTs_zoom.png)

#### Endgadgement

![Ejemplo de Endgadgement](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_endgadgement_zoom.png)

#### Most frequent words

![Ejemplo de Palabras más frecuentes](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_words_frequency_zoom.png)

#### Most frequent retweets

![Ejemplo de Perfiles más retuiteados](https://github.com/congosto/congosto.github.io/raw/master/elonmusk_retweets_frequency_zoom.png)
