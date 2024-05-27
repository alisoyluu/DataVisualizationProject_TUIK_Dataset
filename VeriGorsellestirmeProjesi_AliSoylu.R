install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("treemapify")
install.packages("gridExtra")
install.packages("gglotify")
install.packages("patchwork")
install.packages("ggcharts")
install.packages("devtools")
devtools::install_github("htastan/TRmaps")
install.packages("sf")




library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(treemapify)
library(gridExtra)
library(ggplotify)
library(patchwork)
library(ggcharts)
library(devtools)
library(sf)

## 4.Grafik ## 

sadece_2023_egitim <- X2_EbeveynEgitim_KisiEgitim[X2_EbeveynEgitim_KisiEgitim$yil ==2023, ] # Verisetinden yalnızca 2023 yılı verilerini çektim.

ggplot(sadece_2023_egitim) + # sadece_2023_egitim veri çerçevesini temel alan bir ggplot nesnesi oluşturdum.
  aes(x = ebeveyn_egitim, y = yuzde_deger, fill = kisi_egitim) + # x eksenini ebeveyn_egitim değişkeninden, y eksenini yuzde_deger değişkeninden ve dolgu rengini kisi_egitim değişkeninden alan bir estetik oluşturdum.
  geom_col() + # Grafiğin sütunlarını oluşturdum.
  geom_text(aes(label = paste0("%", yuzde_deger)), position = position_stack(vjust = 0.5), color = "black", size = 4) + # Her sütunun üzerine yüzde değerlerini eklemek için geom_text kullandım. 
  scale_fill_brewer(palette = "Pastel1", name = "Kişi Eğitim Durumu :") + # Dolgu renklerini ayarlamak için scale_fill_brewer kullandım. "Pastel1" renk paletini seçtim.
  theme_bw() + # Temayı theme_bw() olarak ayarladım.
  labs(y = "", x = "Ebeveyn Eğitim Durumu") + # Eksen ve başlık etiketlerini tanımladım. Y ekseninin etiketini boş olarak atadım, X ekseninin etiketini "Ebeveyn Eğitim Durumu" olarak atadım.
  facet_wrap(vars(ebeveyn), scales = "free") + # Verileri ebeveyn değişkeninin değerlerine göre ayrılmış çoklu paneller halinde göstermek için facet_wrap kullandım. 
  theme(strip.text = element_text(hjust = 0.5, size = 12), # Panel başlıkları ve grafik başlığını ortaladım, panel ızgaralarını kaldırdım ve Y eksenindeki metin ve işaret çizgilerini kaldırdım. X eksenindeki metinlerin puntosunu ayarladım.
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12), # Renk skalası etiketlerinin boyutunu 12 olarak ayarladım.
        legend.title = element_text(size = 14), # Renk skalası başlığının boyutunu 14 olarak ayarladım.
        legend.position = "bottom") + # Renk skalasını x ekseninin altına taşıdım.
  labs(title = "Eğitim Seviyesinin Kuşaklararası Aktarımı") + # Başlık eklemek için labs() fonksiyonunu kullanıyoruz
  theme(plot.title = element_text(size = 18))




## 3.Grafik ##

sadece_2023_gelir <- X6_14yasMaddiDurum_SuankiMaddiDurum[X6_14yasMaddiDurum_SuankiMaddiDurum$yil ==2023, ] # Verisetinden yalnızca 2023 yılı verilerini çektim.

sadece_2023_gelir$suanki_gelir_grubu <- factor(sadece_2023_gelir$suanki_gelir_grubu, 
                                               levels = c("İlk Dilim (En Düşük)", "İkinci Dilim", "Üçüncü Dilim", "Dördüncü Dilim", "Son Dilim (En Yüksek)"))

ggplot(sadece_2023_gelir, aes(area = yuzde_deger, 
                              fill = suanki_gelir_grubu, 
                              label = suanki_gelir_grubu,
                              subgroup = ondortyas_hane_maddi_durum)) +
  geom_treemap() +
  geom_treemap_text(aes(label = paste("%", yuzde_deger)),
                    colour = "black",
                    place = "centre",
                    size = 12) +
  geom_treemap_subgroup_text(data = unique(sadece_2023_gelir),  
                             aes(x = NULL, y = NULL,),
                             place = "center",
                             size = 40,
                             colour = "white",
                             fontface = "bold",
                             alpha = 0.7) + 
  geom_treemap_subgroup_border(colour = "white", 
                               size = 5) +
  scale_fill_manual(values = c("İlk Dilim (En Düşük)" = "#FBB4AE", 
                               "İkinci Dilim" = "#B3CDE3", 
                               "Üçüncü Dilim" = "#CCEBC5", 
                               "Dördüncü Dilim" = "#E5D8BD", 
                               "Son Dilim (En Yüksek)" = "#DECBE4"), 
                    name = "Kişi Gelir Dilimi,2023:") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),  
        axis.title.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),  
        axis.ticks = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0.5)) +
  labs(title = "Gelir Durumunun Kuşaklararası Aktarımı",
       caption = "***Kişiler 14 yaşındayken ailelerinin maddi durumları çok kötü, biraz kötü, kötü, biraz iyi, iyi, çok iyi olmak üzere 6 farklı gruba ayrılmıştır.") +
  theme(plot.title = element_text(size = 18))




## 6.Grafik ##

sadece_2023_mulkiyet <-X7_14yasHaneMulkiyet_SuanHaneMulkiyet[X7_14yasHaneMulkiyet_SuanHaneMulkiyet$yil ==2023, ]

ggplot(sadece_2023_mulkiyet, aes(x = yuzde_deger, y = ebEveyn_hane_mulkiyeti, fill = kisi_hane_mulkiyeti)) +
  geom_bar(stat = "identity") +
  geom_text(data = subset(sadece_2023_mulkiyet, yuzde_deger >= 5),
            aes(label = paste0("%", yuzde_deger)), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  labs(x = "", y = "Ebeveyn Konut Mülkiyet Durumu", fill = "Kişi Mülkiyet Durumu :") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(strip.text = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),  
        legend.position = "bottom") + 
  labs(title = "Mülkiyet Durumunun Kuşaklararası Aktarımı") + 
  theme(plot.title = element_text(size = 18))




## 7.Grafik ##

sadece_2023_tatil <-X5_14YasTatil_SuanTatil[X5_14YasTatil_SuanTatil$yil ==2023, ]

ggplot(sadece_2023_tatil, aes(x = ondortyas_hane_uzakta_tatil_yapabilme_durumu, y = yuzde_deger, fill = suan_hane_uzakta_tatil_yapabilme_durumu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Kişi On Dört Yaşında İken Ailesiyle Tatil Yapabilme Durumu", y = "", fill = "Kişinin 2023 Yılı Tatil Yapabilme Durumu:") +
  ggtitle("") +
  scale_fill_manual(values = c("#CCEBC5", "#FBB4AE")) +  
  theme_bw() +
  geom_text(aes(label = paste0( "%", yuzde_deger)), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(strip.text = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Tatil Masrafı Karşılama Durumunun Kuşaklararası Aktarımı") + 
  theme(plot.title = element_text(size = 18))




## 8.Grafik ##

sadece_2023_et <-X4_14YasEtYeme_SuanEtYeme[X4_14YasEtYeme_SuanEtYeme$yil ==2023, ]

ggplot(sadece_2023_et, aes(x = yuzde_deger, y = ondortyas_hane_et_yiyebilme_durumu_gunluk, fill = suan_hane_et_yiyebilme_durumu_2gunde1)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0( "%", yuzde_deger)), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("Evet" = "#CCEBC5", "Hayır" = "#FBB4AE")) + 
  labs(x = "", y = "On Dört Yaşında İken Günlük Et Yiyebilme Durumu", fill = "2023 Yılında Kişinin 2 Günde 1 Kez Et Yiyebilme Durumu:") +
  ggtitle("Et Ürünleri Tüketebilme Durumlarının Kuşaklararası Aktarımı ") +  
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 18),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text( size = 14),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.position = "bottom") 




## 5.Grafik ##

pyramid_1 <- X3_EbeveynI_stekiDurumu_KisininI_stekiDurumu %>%
  filter(yil == 2023, ebeveyn_isteki_durumu == "Ücretsiz aile işçisi") %>%
  pyramid_chart(x = kisi_isteki_durumu, y = yuzde_deger, group = ebeveyn, bar_colors = c("#B3CDE3", "#B3CDE3"),
                xlab = "Kişinin İş Durumu(%)",
                title = "Ebeveynleri Ücretsiz Aile İşçisi Olanlar")

pyramid_1_ggplot <- ggplotify::as.ggplot(pyramid_1)

pyramid_2 <- X3_EbeveynI_stekiDurumu_KisininI_stekiDurumu %>%
  filter(yil == 2023, ebeveyn_isteki_durumu == "İktisaden faal olmayan") %>%
  pyramid_chart(x = kisi_isteki_durumu, y = yuzde_deger, group = ebeveyn, bar_colors = c("#FBB4AE","#FBB4AE"),
                xlab = "Kişinin İş Durumu(%)",
                title = "Ebeveynleri İktisaden Faal Olmayanlar")

pyramid_2_ggplot <- ggplotify::as.ggplot(pyramid_2)

pyramid_3 <- X3_EbeveynI_stekiDurumu_KisininI_stekiDurumu %>%
  filter(yil == 2023, ebeveyn_isteki_durumu == "Ücretli,maaşlı,yevmiyeli") %>%
  pyramid_chart(x = kisi_isteki_durumu, y = yuzde_deger, group = ebeveyn, bar_colors = c("#FED9A6", "#FED9A6"),
                xlab = "Kişinin İş Durumu(%)",
                title = "Ebeveynleri Ücretli, Maaşlı,Yevmiyeli Olanlar")

pyramid_3_ggplot <- ggplotify::as.ggplot(pyramid_3)

pyramid_4 <- X3_EbeveynI_stekiDurumu_KisininI_stekiDurumu %>%
  filter(yil == 2023, ebeveyn_isteki_durumu == "İşveren/Kendi hesabına") %>%
  pyramid_chart(x = kisi_isteki_durumu, y = yuzde_deger, group = ebeveyn, bar_colors = c("#CCEBC5", "#CCEBC5"),
                xlab = "Kişinin İş Durumu(%)",
                title = "Ebeveynleri İşveren/Kendi Hesabına Çalışanlar")

pyramid_4_ggplot <- ggplotify::as.ggplot(pyramid_4)

color_scale <- scale_fill_manual(values = c("#B3CDE3", "#FBB4AE", "#FED9A6", "#CCEBC5"),
                                 labels = c("Ücretsiz aile işçisi", "İktisaden faal olmayan", 
                                            "Ücretli, maaşlı, yevmiyeli", "İşveren/Kendi hesabına"))

combined_plot <- (pyramid_1_ggplot | pyramid_2_ggplot) / (pyramid_3_ggplot | pyramid_4_ggplot)


combined_plot



## 2.Grafik ##

X8_Gini_Katsayilari_2014to2023 <- subset(X8_Gini_Katsayilari_2006to2023, yil >= 2014 & yil <= 2023)

ggplot(X8_Gini_Katsayilari_2014to2023, aes(x = yil, y = gini_katsayisi)) +
  geom_line(color = ifelse(X8_Gini_Katsayilari_2014to2023$yil >= 2014, "red", "black")) +
  geom_point(size = ifelse(X8_Gini_Katsayilari_2014to2023$yil %in% c(2014, 2023), 3, 2)) + 
  geom_text(data = subset(X8_Gini_Katsayilari_2014to2023, yil %in% c(2014, 2023)), aes(label = gini_katsayisi), vjust = 2, hjust = 0.25) +
  labs(x = "Yıl", y = "Gini Katsayısı", title = "Türkiye'de Gelir Dağılımı Eşitsizliği: Gini Katsayısı", 
       caption = "***Gelir dağılımı eşitsizliği ölçütlerinden olan Gini katsayısı, sıfıra yaklaştıkça gelir dağılımında eşitliği, bire yaklaştıkça gelir dağılımında bozulmayı ifade etmektedir.") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.35, 0.45)) +
  scale_x_continuous(breaks = seq(min(X8_Gini_Katsayilari_2014to2023$yil), max(X8_Gini_Katsayilari_2014to2023$yil), by = 1), minor_breaks = NULL) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 1)) +
  theme(plot.title = element_text(size = 18))


##1.Grafik ##

install.packages("devtools")
library(devtools)

devtools::install_github("htastan/TRmaps")
library(TRmaps)

install.packages("sf")
library(sf)

data("tr_nuts3")

install.packages("dplyr")
library(dplyr)

tr_gelir_2023 <- X9_Harita_fert_gelir_bolgeler |> 
  select(name_eng, ortalama_fert_basina_gelir_yillik_2023, il_grubu)

tr_gelir_2023_2 <- left_join(tr_nuts3, tr_gelir_2023, by = c("name_eng" = "name_eng"))

ggplot(tr_gelir_2023_2) + 
  geom_sf(aes(fill = ortalama_fert_basina_gelir_yillik_2023)) +
  theme_void()

tr_gelir_2023_2$ortalama_fert_basina_gelir_yillik_2023 <- as.numeric(tr_gelir_2023_2$ortalama_fert_basina_gelir_yillik_2023)

tr_gelir_2023_2$gelir_araligi <- cut(tr_gelir_2023_2$ortalama_fert_basina_gelir_yillik_2023,
                                     breaks = c(0, 39000, 54000, 69000, 84000, 99000, 115000),
                                     labels = c("Deprem sebebiyle veri elde edilememiştir.", "39000-53999", "54000-68999", "69000-83999", "84000-98999", "99000-115000"))

library(ggplot2)

ggplot(tr_gelir_2023_2) + 
  geom_sf(aes(fill = gelir_araligi)) +
  geom_sf_text(aes(label = name_tr), size = 2) +  
  scale_fill_manual(name = "            Fert Başına Yıllık Ortalama Gelir (TL)", 
                    values = c("Deprem sebebiyle veri elde edilememiştir." = "#F2F2F2",
                               "39000-53999" = "#FBB4AE", 
                               "54000-68999" = "#FFFFCC", 
                               "69000-83999" = "#FED9A6", 
                               "84000-98999" = "#B3CDE3",
                               "99000-115000" = "#CCEBC5"),
                    guide = guide_legend(position = "bottom", ncol = 2, title.position = "top")) +
  theme_void() +
  theme(legend.position = "bottom", legend.justification = "right",
        plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 15)) +
  labs(title = "Yıllık Ortalama Eşdeğer Hanehalkı Kullanılabilir Fert Geliri (TL)",
       fill = "Yıllık Ortalama Gelir (TL)",
       caption = "***Ülkemizde yaşanan deprem nedeni ile 2023 yılında Hatay, Kahramanmaraş, Osmaniye illerinde alan çalışması yapılamadığı için buralardan veri sağlanamamıştır.") +
  theme(plot.title = element_text(size = 18))



# Pastel1 Paletindeki Renklerin Kodları: "#FBB4AE"-(pembe), "#B3CDE3"-(açıkmavi), "#CCEBC5"-(yesil), "#DECBE4"-(pembe), "#FED9A6"-(kavun), "#FFFFCC"-(sarı), "#E5D8BD"-(sütlükahve), "#FDDAEC"-(pembe), "#F2F2F2"-(açıkgri)




