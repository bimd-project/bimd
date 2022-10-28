
![BIMD2011](https://user-images.githubusercontent.com/104343943/198562800-803a0298-951f-48ca-bacc-aada780d2164.jpg)
                            <div align="center">
  From the most --------------------------------------------------------------------------- to the least deprived
</div>




## The Belgian Indices of Multiple Deprivation

**The Belgian indices of multiple deprivation** are spatial- and time-specific measures of relative deprivation on the level of the smallest geographical unit in Belgium, the statistical sector. The development of the deprivation indices was one of the main goals of the ELLIS project (https://www.brain-ellis.be/) and was funded by the Belgian Federal Science Policy (BELSPO) through the BRAIN-be 2.0 programme. 

The indices were developed for the years 2001 (**BIMD2001**) and 2011 (**BIMD2011**) and they assign the statistical sectors into deprivation deciles, from 1 (the most deprived 10 percent of statistical sectors nationally) to 10 (the least deprived 10 percent). 

The indices combine information from six deprivation domains, measuring different types of dimensions of deprivation, such as income, employment, education, housing, health and crime. The building blocks of these domains are indicators, which are either rates or proportions of population in a given statistical sector experiencing some kind of deprivation. For instance, rate of drug related crimes, standardized suicide rate, or a proportion of working age population who is unemployed.  

The domains are combined to produce the overall index of multiple deprivation scores, which are then ranked and assign to deprivation deciles. The range of measures we provide here include scores, ranks, and deciles for each deprivation domain and the overall BIMDs. Higher geographical level summary measures are also available for municipalities and they include average score, average rank, extent, and the proportion of statistical sectors in the most deprived 10 percent nationally on the municipal level. 

## What is the use of the domains and the BIMD2001 and the BIMD2011?
The deprivation domains can be used on their own to investigate specific aspects of deprivation (e.g. income or education), whereas the BIMD2001 and BIMD2011 can be used for researching the overall socio-economic deprivation. If a health outcome is the main research interest, we recommend to use the overall BIMD2001 and BIMD2011 built without the health deprivation domain. 
The ranks can be directly compared – the closer the statistical sector’s rank is to 1, the more deprived it is. To identify the most deprived statistical sectors nationally (by a specific domain or the BIMDs) the published deciles can be used. It is important to note that the indices are not designed to provide a comparability in time – they provide a comparison based on the rankings as determined at a certain time point by each version of the index. For instance, a statistical sector can be said to have become less deprived relative to other statistical sectors if it was within the most 10 percent of statistical sectors nationally according to the BIMD2001, but within the most deprived 30 percent according to the BIMD2011.  

## What data files and supporting materials are available? 
###### Data 
We publish datasets at the level of the statistical sector and municipality. The former contains scores, ranks and deciles for all deprivation domains and for the overall BIMD2001 and BIMD2011. The latter contains a higher-area level summary measure that is population-weighted, an average score. The average scores are provided for deprivation domains and the overall BIMDs. And as the name suggests, these measures are based on either the domain' or the overall BIMDs' scores. These files also contain the rank of average score, and deprivation decile based on the average score. All files are available in the long and wide format. 


###### R codes
There are number of R files containing codes that were applied to create the domains and overall BIMD2001 and the BIMD2011. Althouth we are not able to publish the raw data due to privacy issue, you are encouraged to inspect our codes and methodology. Using our codes and published dataset, you are, however, able to make your own changes in the BIMD2001 and BIMD2011. For instance, you can re-calculate the overall indices without using the health domain, if the research interest is in investigating the health inequalities. You can also reassign the ranks into quantiles rather than deciles. In addition, we provide R codes for calculating the summary measures at the level of municipality that includes average score, average rank and extent. 

- File 1 BIMD2001 DOMAINS (SCORES, RANKS, DECILE)
- File 2 BIMD2011 DOMAINS (SCORES, RANKS, DECILE)
- File 3 Shapefiles  
- File 4 Diagram of indicators for the BIMD2001 and BIMD2011
- File 5 Summary measures at the level of municipality for the BIMD2001
- File 6 Summary measures at the level of municipality for the BIMD2011
- File 7 R codes 
