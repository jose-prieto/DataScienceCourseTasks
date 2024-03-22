#Primer paso: importar data frame
df = read.csv("C:\\Users\\jap25\\OneDrive\\Documentos\\git workspace\\DataScienceCourseTasks\\Task #3 - Exploratory data analysis in R\\Entregable 3\\fundamentals.csv", sep=",", header=TRUE)
head(df)
dim(df)

#Procedemos a homologar y limpiar los nombres de las columnas quitando espacios en blanco innecesarios y los necesarios los cambiamos a "_"
library(janitor)
df = clean_names(df)
head(df)
df$period_ending <- as.Date(df$period_ending, format = "%Y-%m-%d")

# Tal y como se hizo en el ejercicio anterior, iteramos entre las columnas para hacer la limpieza correspondiente de cada una
string_columns <- sapply(df, is.character)

df[string_columns] <- lapply(df[string_columns], function(col) {
  col <- trimws(col) # Elimina espacios al inicio y al final
  col <- tolower(col) # Convierte a minúsculas
  col <- gsub(" ", "_", col) # Reemplaza espacios por guiones bajos
  return(col)
})

#Verificamos los tipos de datos de las columnas para ver si está acorde a lo que necesitamos
str(df)

"""En este punto podremos definir a que se refiere cada columna:
Ticker Symbol: Símbolo de Cotización
Period Ending: Fin del Período
Accounts Payable: Cuentas por Pagar
Accounts Receivable: Cuentas por Cobrar
Add'l income/expense items: Elementos Adicionales de Ingresos/Gastos
After Tax ROE: ROE después de Impuestos (Retorno sobre el Capital)
Capital Expenditures: Gastos de Capital
Capital Surplus: Superávit de Capital
Cash Ratio: Ratio de Liquidez
Cash and Cash Equivalents: Efectivo y Equivalentes de Efectivo
Changes in Inventories: Cambios en Inventarios
Common Stocks: Acciones Comunes
Cost of Revenue: Costo de Ingresos
Current Ratio: Ratio de Liquidez Corriente
Deferred Asset Charges: Cargos por Activos Diferidos
Deferred Liability Charges: Cargos por Pasivos Diferidos
Depreciation: Depreciación
Earnings Before Interest and Tax: Ganancias antes de Intereses e Impuestos
Earnings Before Tax: Ganancias antes de Impuestos
Effect of Exchange Rate: Efecto del Tipo de Cambio
Equity Earnings/Loss Unconsolidated Subsidiary: Ganancias/Pérdidas de Capital de Filial No Consolidada
Fixed Assets: Activos Fijos
Goodwill: Fondo de Comercio
Gross Margin: Margen Bruto
Gross Profit: Beneficio Bruto
Income Tax: Impuesto sobre la Renta
Intangible Assets: Activos Intangibles
Interest Expense: Gastos por Intereses
Inventory: Inventario
Investments: Inversiones
Liabilities: Pasivos
Long-Term Debt: Deuda a Largo Plazo
Long-Term Investments: Inversiones a Largo Plazo
Minority Interest: Interés Minoritario
Misc. Stocks: Acciones Diversas
Net Borrowings: Préstamos Netos
Net Cash Flow: Flujo de Caja Neto
Net Cash Flow-Operating: Flujo de Caja Neto de Operaciones
Net Cash Flows-Financing: Flujos de Caja Netos de Financiamiento
Net Cash Flows-Investing: Flujos de Caja Netos de Inversión
Net Income: Ingreso Neto
Net Income Adjustments: Ajustes al Ingreso Neto
Net Income Applicable to Common Shareholders: Ingreso Neto Aplicable a Accionistas Comunes
Net Income-Cont. Operations: Ingreso Neto de Operaciones Continuas
Net Receivables: Cuentas por Cobrar Netas
Non-Recurring Items: Elementos No Recurrentes
Operating Income: Ingreso Operativo
Operating Margin: Margen Operativo
Other Assets: Otros Activos
Other Current Assets: Otros Activos Corrientes
Other Current Liabilities: Otras Pasivos Corrientes
Other Equity: Otras Participaciones en el Capital
Other Financing Activities: Otras Actividades de Financiamiento
Other Investing Activities: Otras Actividades de Inversión
Other Liabilities: Otros Pasivos
Other Operating Activities: Otras Actividades Operativas
Other Operating Items: Otros Elementos Operativos
Pre-Tax Margin: Margen antes de Impuestos
Pre-Tax ROE: ROE antes de Impuestos
Profit Margin: Margen de Beneficio
Quick Ratio: Ratio de Prueba Ácida
Research and Development: Investigación y Desarrollo
Retained Earnings: Ganancias Retenidas
Sale and Purchase of Stock: Venta y Compra de Acciones
Sales, General and Admin.: Ventas, General y Administración
Short-Term Debt / Current Portion of Long-Term Debt: Deuda a Corto Plazo / Porción Corriente de la Deuda a Largo Plazo
Short-Term Investments: Inversiones a Corto Plazo
Total Assets: Activos Totales
Total Current Assets: Activos Corrientes Totales
Total Current Liabilities: Pasivos Corrientes Totales
Total Equity: Patrimonio Total
Total Liabilities: Pasivos Totales
Total Liabilities & Equity: Total de Pasivos y Patrimonio
Total Revenue: Ingresos Totales
Treasury Stock: Acciones de Tesorería
For Year: Para el Año
Earnings Per Share: Ganancias por Acción
Estimated Shares Outstanding: Acciones Estimadas en Circulación

E identificamos como variables objetivo:
Para el siguiente ejercicio identifico como importante el margen de beneficio neto, lo cual, se calcula dividiendo el ingreso
neto entre los ingresos totales, por ende, nuestras dos variables objetivos serían:

* Total Revenue (Ingresos Totales): Una variable clave para el análisis fundamental, los ingresos totales pueden indicar la 
salud general y la tendencia de crecimiento de una empresa. Analizar cómo los ingresos afectan los precios de las acciones 
podría ser crucial para comprender si la negociación automatizada puede identificar oportunidades de inversión basadas en 
el crecimiento de ingresos.

* Net Income (Ingreso Neto): Representa las ganancias totales después de deducir gastos e impuestos. Esta variable es un 
indicador significativo del éxito financiero de una empresa y puede ser vital para entender cómo las máquinas interpretan 
y reaccionan a los cambios en la rentabilidad."""

print(colSums(is.na(df)))

df$beneficioNeto <- df$net_income / df$total_revenue
head(df)

hist(df$beneficioNeto, freq = TRUE)

df$beneficioNeto = log1p(df$beneficioNeto)

hist(df$beneficioNeto, freq = TRUE)
