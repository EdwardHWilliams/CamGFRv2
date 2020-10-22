fpval.txt <- function(pval) {
    if (pval == "<0.001") 
        pval.txt <- "P<0.001" else {
            pval <- as.numeric(pval)
            if (pval < 0.01) {
                pval.txt <- paste("P=", round(pval, 3), sep = "")
            } else if (pval > 0.045 & pval < 0.05) {
                pval.txt <- paste("P=", round(pval, 3), sep = "")
            } else {
                pval.txt <- paste("P=", round(pval, 3), sep = "")
            }
        }
    return(pval.txt)
}

fsmry.by.grp <- function(y, grp, log.tr = F, method = "Tukey", method.padj = NULL, paired = F, IQR = T) {
    if (log.tr) 
        y <- log(y)
    if (!is.factor(grp)) 
        grp <- factor(grp)
    n <- table(grp)
    n.cmplt <- tapply(y, grp, function(x) sum(!is.na(x)))
    mean.sd <- tapply(y, grp, function(x) paste(round(mean(x, na.rm = T), 2), " +/- ", round(sd(x, na.rm = T), 2), sep = ""))
    if (IQR) 
        median.IQR <- tapply(y, grp, function(x) paste(round(median(x, na.rm = T), 2), " (", round(quantile(x, p = 0.25, na.rm = T), 2), ",", round(quantile(x, p = 0.75, na.rm = T), 2), ")", sep = "")) 
    else median.range <- tapply(y, grp, function(x) paste(round(median(x, na.rm = T), 2), " (", round(min(x, na.rm = T), 2), ",", round(max(x, na.rm = T), 2), ")", sep = ""))
    if (length(levels(grp)) > 2) {
        pval.aov <- anova(lm(y ~ grp))[1, "Pr(>F)"]
        multcmp.cname <- NULL
        if (is.null(method.padj)) 
            method.padj <- "holm"
        if (length(method) > 1) {
            if (method.padj != "none") {
                multcmp.out <- data.frame(summary(glht(lm(y ~ grp), linfct = mcp(grp = method)), test = adjusted(method.padj))$test[3:6])
                multcmp.cname <- c(multcmp.cname, paste("pm.p.", method.padj, sep = ""))
                multcmp.out <- cbind(multcmp.out, data.frame(summary(glht(lm(y ~ grp), linfct = mcp(grp = method)), test = adjusted("none"))$test[3:6]))
            } else multcmp.out <- data.frame(summary(glht(lm(y ~ grp), linfct = mcp(grp = method)), test = adjusted("none"))$test[3:6])
            multcmp.cname <- c(multcmp.cname, "pm.p")
        } else {
            multcmp.out <- data.frame(summary(glht(lm(y ~ grp), linfct = mcp(grp = method)))$test[3:6])
            multcmp.cname <- c(multcmp.cname, paste("pm.p.", method, sep = ""))
        }
        names.comp <- rownames(multcmp.out)
        pval.KW <- kruskal.test(y ~ grp)$p.value
        p.wilcox <- rep(NA, length(names.comp))
        if (length(method) == 1) 
            p.t <- rep(NA, length(names.comp))
        for (i in 1:length(names.comp)) {
            grp.names.signs <- strsplit(names.comp[i], split = " ")[[1]]
            grp.names.all <- grp.names.signs[-which(grp.names.signs %in% c("+", "-"))]
            grp.names2 <- grp.names.signs[which(grp.names.signs == "-") + 1]
            grp.names1 <- grp.names.all[-which(grp.names.all %in% grp.names2)]
            y1 <- y[grp == grp.names1]
            y2 <- y[grp == grp.names2]
            p.wilcox[i] <- wilcox.test(y1, y2, exact = F)$p.value
            if (length(method) == 1) {
                if (length(y1) == 1 & length(y2) == 1) 
                    p.t[i] <- 1 else if (length(y1) == 1 | length(y2) == 1) 
                        p.t[i] <- t.test(y1, y2, var.equal = T)$p.value else p.t[i] <- t.test(y1, y2, var.equal = F)$p.value
            }
        }
        p.wilcox.adj <- p.adjust(p.wilcox, method = method.padj)
        if (length(method) == 1) {
            if (method.padj == "none") {
                multcmp.out <- data.frame(multcmp.out, p.t, p.wilcox)[, 4:6]
                multcmp.cname <- c(multcmp.cname, "pm.p", "wilcoxon.p")
            } else {
                multcmp.out <- data.frame(multcmp.out, p.t, p.wilcox.adj, p.wilcox)[, 4:7]
                multcmp.cname <- c(multcmp.cname, "pm.p", paste("wilcoxon.p.", method.padj, sep = ""), "wilcoxon.p")
            }
        } else {
            if (method.padj == "none") {
                multcmp.out <- data.frame(multcmp.out, p.wilcox)[, 4:5]
                multcmp.cname <- c(multcmp.cname, "wilcoxon.p")
            } else {
                multcmp.out <- data.frame(multcmp.out, p.wilcox.adj, p.wilcox)[, c(4, 8:10)]
                multcmp.cname <- c(multcmp.cname, paste("wilcoxon.p.", method.padj, sep = ""), "wilcoxon.p")
            }
        }
        dimnames(multcmp.out)[[2]] <- multcmp.cname
        pvalue.pairwise.comp <- data.frame(apply(multcmp.out, 2, function(x) ifelse(x <= 0.001, "<0.001", round(x, 3))))
        if (IQR) {
            out <- data.frame(n = as.numeric(n), 
                              n.complete = n.cmplt, 
                              mean.sd = mean.sd, 
                              pval.anova = c(rep("", length(n) - 1), ifelse(pval.aov < 0.001, "<0.001", round(pval.aov, 3))), 
                              median.IQR = median.IQR, pval.KW = c(rep("", length(n) - 1), ifelse(pval.KW < 0.001, "<0.001", round(pval.KW, 3))))
            out2 <- data.frame(n = as.numeric(n), 
                               n.complete = n.cmplt, 
                               mean.sd = mean.sd, 
                               pval.anova = c(rep("", length(n) - 1), pval.aov), 
                               median.IQR = median.IQR, 
                               pval.KW = c(rep("", length(n) - 1), pval.KW), stringsAsFactors = F)
        } else {
            out <- data.frame(n = as.numeric(n), 
                              n.complete = n.cmplt, 
                              mean.sd = mean.sd, 
                              pval.anova = c(rep("", length(n) - 1), ifelse(pval.aov < 0.001, "<0.001", round(pval.aov, 3))), 
                              median.range = median.range, 
                              pval.KW = c(rep("", length(n) - 1), ifelse(pval.KW < 0.001, "<0.001", round(pval.KW, 3))))
            out2 <- data.frame(n = as.numeric(n), 
                               n.complete = n.cmplt, 
                               mean.sd = mean.sd, 
                               pval.anova = c(rep("", length(n) - 1), pval.aov), 
                               median.range = median.range, 
                               pval.KW = c(rep("", length(n) - 1), pval.KW), stringsAsFactors = F)
        }
        attr(out, "detailed") <- list(summary = out2, pval.pairwise.comp = multcmp.out)
        return(list(summary = out, pval.pairwise.comp = pvalue.pairwise.comp))
    } else {
        grp.levels <- levels(grp)
        pval.t <- t.test(y[grp == grp.levels[1]], y[grp == grp.levels[2]], paired = paired, na.action = na.omit)$p.val
        pval.w <- wilcox.test(y[grp == grp.levels[1]], y[grp == grp.levels[2]], paired = paired, na.action = na.omit)$p.val
        if (IQR) {
            out <- data.frame(n = as.numeric(n), 
                              n.complete = n.cmplt, 
                              mean.sd = mean.sd, 
                              pval.t = c(rep("", length(n) - 1), ifelse(pval.t < 0.001, "<0.001", round(pval.t, 3))), 
                              median.IQR = median.IQR, pval.W = c(rep("", length(n) - 1), ifelse(pval.w < 0.001, "<0.001", round(pval.w, 3))))
            out2 <- data.frame(n = as.numeric(n), 
                               n.complete = n.cmplt, 
                               mean.sd = mean.sd, 
                               pval.t = c(rep("", length(n) - 1), pval.t), 
                               median.IQR = median.IQR, pval.W = c(rep("", length(n) - 1), pval.w), stringsAsFactors = F)
        } else {
            out <- data.frame(n = as.numeric(n), 
                              n.complete = n.cmplt, 
                              mean.sd = mean.sd, 
                              pval.t = c(rep("", length(n) - 1), ifelse(pval.t < 0.001, "<0.001", round(pval.t, 3))), 
                              median.range = median.range, 
                              pval.W = c(rep("", length(n) - 1), ifelse(pval.w < 0.001, "<0.001", round(pval.w, 3))))
            out2 <- data.frame(n = as.numeric(n), 
                               n.complete = n.cmplt, 
                               mean.sd = mean.sd, 
                               pval.t = c(rep("", length(n) - 1), pval.t), 
                               median.range = median.range, 
                               pval.W = c(rep("", length(n) - 1), pval.w), stringsAsFactors = F)
        }
        attr(out, "detailed") <- out2
        return(out)
    }
}


fsmry2.by.grp <- function(y, grp, cmp.method = c("fisher", "chisq")) {
    y <- y[!is.na(grp)]
    grp <- grp[!is.na(grp)]
    n.y <- table(y)
    n.missing <- sum(is.na(y))
    if (n.missing > 0) 
        n.mis.tab <- table(ifelse(is.na(y), "Yes", "No"))
    y.by.grp <- table(y, grp)
    if (cmp.method == "fisher") {
        test.out <- fisher.test(y.by.grp)
        if (n.missing > 0) {
            mis.by.grp <- table(ifelse(is.na(y), "Yes", "No"), grp)
            test2.out <- fisher.test(mis.by.grp)
        }
    }
    if (cmp.method == "chisq") {
        test.out <- chisq.test(y.by.grp)
        if (n.missing > 0) {
            mis.by.grp <- table(ifelse(is.na(y), "Yes", "No"), grp)
            test2.out <- chisq.test(mis.by.grp)
        }
    }
    out.y <- data.frame(n = as.numeric(n.y), apply(y.by.grp, 2, function(x) paste(x, " (", round(100 * x/sum(x), 2), "%)", sep = "")), p.value = c(rep("", length(n.y) - 1), ifelse(test.out$p.value < 0.001, "<0.001", round(test.out$p.value, 3))))
    row.names(out.y) <- row.names(y.by.grp)
    attr(out.y, "detailed") <- test.out
    if (n.missing > 0) {
        out.missing <- data.frame(n = as.numeric(n.mis.tab), apply(mis.by.grp, 2, function(x) paste(x, " (", round(100 * x/sum(x), 2), "%)", sep = "")), p.value = c(rep("", length(n.mis.tab) - 1), ifelse(test2.out$p.value < 0.001, "<0.001", round(test2.out$p.value, 3))))
        row.names(out.missing) <- row.names(mis.by.grp)
        attr(out.missing, "detailed") <- test2.out
        return(list(summary = out.y, missingness = out.missing))
    } else return(out.y)
}



fsmry.dmgrph <- function (dat = dat.work, vars = vars, vars.cat = vars.cat, vars.chisq = rep(0, length(vars)), by = "fit_index", all = T, markdown = T, IQR = T) {
    id.na <- which(is.na(dat[, by]))
    if (length(id.na) > 0) 
        dat <- dat[-id.na, ]
    n <- dim(dat)[1]
    out <- NULL
    if (all) {
        out.all <- vector(length = length(vars), mode = "list")
        names(out.all) <- vars
    }
    if (!is.null(by)) {
        out.by.grp <- vector(length = length(vars), mode = "list")
        names(out.by.grp) <- paste(vars, " ~ ", by, sep = "")
        dat[, by] <- factor(dat[, by])
        level.by <- levels(dat[, by])
        n.level <- length(level.by)
        n.by.cat <- table(dat[, by])
    }
    for (i in 1:length(vars)) {
        cat(paste(i, ". ", vars[i], "\n", sep = ""))
        tmp <- NULL
        if (vars.cat[i]) {
            if (all) {
                if (sum(is.na(dat[, vars[i]])) == 0) 
                    out.all[[i]] <- cbind(n = table(dat[, vars[i]]), 
                                          prop = prop.table(table(dat[, vars[i]])))
                else {
                    n.missing <- sum(is.na(dat[, vars[i]]))
                    out.all[[i]] <- cbind(n = c(table(dat[, vars[i]]), missing = n.missing), 
                                          prop = c(prop.table(table(dat[,vars[i]])), missing = n.missing/n))
                }
                
                tmp <- rbind(c(ifelse(markdown, paste("**", vars[i], ", n (%)**", sep = ""), paste(vars[i], ", n (%)", sep = "")), ""), cbind(paste("   ", row.names(out.all[[i]])), paste(out.all[[i]][, "n"], "(", round(out.all[[i]][,"prop"] * 100, 2), "%)", sep = "")))
            }
            if (!is.null(by)) {
                if (!vars.chisq[i]) 
                    out.by.grp[[i]] <- fsmry2.by.grp(y = dat[,vars[i]], grp = dat[, by], cmp.method = "fisher")
                else {
                    out.by.grp[[i]] <- fsmry2.by.grp(y = dat[,vars[i]], grp = dat[, by], cmp.method = "chisq")
                }
                if (is.null(tmp)) {
                    if (sum(is.na(dat[, vars[i]])) == 0) 
                        tmp <- rbind(c(ifelse(markdown, paste("**", vars[i], ", n (%)**", sep = ""), paste(vars[i], ", n (%)", sep = "")), rep("", n.level + 1)), as.matrix(cbind(paste("   ", row.names(out.by.grp[[i]]), sep = ""), as.matrix(out.by.grp[[i]][,-1]))))
                    else tmp <- rbind(c(ifelse(markdown, paste("**", vars[i], ", n (%)**", sep = ""), paste(vars[i], ", n (%)", sep = "")), rep("", n.level + 1)), as.matrix(cbind(c(row.names(out.by.grp[[i]]), "   missing"), rbind(as.matrix(out.by.grp[[i]][[1]][,-1]), as.vector(out.by.grp[[i]][[2]][2, -1])))))
                }
                else {
                    if (sum(is.na(dat[, vars[i]])) == 0) 
                        tmp <- rbind(c(tmp[1, ], rep("", n.level + 1)), as.matrix(cbind(tmp[-1, ], as.matrix(out.by.grp[[i]][,-1]))))
                    else tmp <- rbind(c(tmp[1, ], rep("", n.level + 1)), as.matrix(cbind(tmp[-1, ], rbind(as.matrix(out.by.grp[[i]][[1]][, -1]), as.vector(out.by.grp[[i]][[2]][2, -1])))))
                }
            }
            out <- rbind(out, tmp)
        }
        else {
            if (all) {
                if (IQR) 
                    out.all[[i]] <- c(mean = mean(dat[, vars[i]], na.rm = T), 
                                      sd = sd(dat[, vars[i]], na.rm = T), 
                                      median = median(dat[, vars[i]], na.rm = T), 
                                      iqr.L = quantile(dat[, vars[i]], p = 0.25, na.rm = T), 
                                      iqr.U = quantile(dat[, vars[i]], p = 0.75, na.rm = T))
                else out.all[[i]] <- c(mean = mean(dat[, vars[i]], na.rm = T), 
                                       sd = sd(dat[, vars[i]], na.rm = T), 
                                       median = median(dat[, vars[i]], na.rm = T), 
                                       min = min(dat[, vars[i]], na.rm = T), 
                                       max = max(dat[, vars[i]], na.rm = T))
                tmp <- rbind(c(ifelse(markdown, paste("**", vars[i], "**", sep = ""), vars[i]), ""), c("   Mean+/-sd", paste(round(out.all[[i]][1], 2), "+/-", round(out.all[[i]][2], 2), sep = "")), c(ifelse(IQR, "   Median (IQR)", "   Median (range)"), paste(round(out.all[[i]][3], 2), " (", round(out.all[[i]][4], 2), ", ", round(out.all[[i]][5], 2), ")", sep = "")))
                if (sum(is.na(dat[, vars[i]])) > 0) {
                    n.missing <- sum(is.na(dat[, vars[i]]))
                    out.all[[i]] <- c(out.all[[i]], n.missing = n.missing, prop.missing = n.missing/n)
                    tmp <- rbind(tmp, c("   missing, n (%)", paste(out.all[[i]]["n.missing"], " (", round(100 * out.all[[i]]["prop.missing"], 2), "%)", sep = "")))
                }
            }
            if (!is.null(by)) {
                out.by.grp[[i]] <- fsmry.by.grp(y = dat[, vars[i]], grp = dat[, by], log.tr = F, IQR = IQR)
                if (length(level.by) == 2) 
                    obg <- out.by.grp[[i]]
                else obg <- out.by.grp[[i]][[1]]
                if (is.null(tmp)) {
                    tmp <- rbind(c(ifelse(markdown, paste("**", vars[i], "**", sep = ""), vars[i]), rep("", n.level + 1)), c("   Mean+/-sd", as.vector(obg[, 3]), as.vector(obg[n.level, 4])), c(ifelse(IQR, "   Median (IQR)", "   Median (range)"), as.vector(obg[, 5]), as.vector(obg[n.level, 6])))
                    if (sum(is.na(dat[, vars[i]])) > 0) {
                        n.missing <- obg[, 1] - obg[, 2]
                        prop.missing <- n.missing/obg[, 1]
                        tmp <- rbind(tmp, c("   missing, n (%)", paste(as.vector(n.missing), " (", as.vector(round(100 * prop.missing, 2)), "%)", sep = "")))
                    }
                }
                else {
                    tmp1 <- rbind(c(tmp[1, ], rep("", n.level + 1)), c(tmp[2, ], as.vector(obg[, 3]), as.vector(obg[n.level, 4])), c(tmp[3, ], as.vector(obg[, 5]), as.vector(obg[n.level, 6])))
                    if (dim(tmp)[1] > 3) {
                        if (any(table(ifelse(is.na(dat[, vars[i]]), "Y", "N"), dat[, by]) <= 5)) 
                            mis.by.grp <- fsmry2.by.grp(y = ifelse(is.na(dat[, vars[i]]), "Y", "N"), grp = dat[, by], cmp.method = "fisher")
                        else mis.by.grp <- fsmry2.by.grp(y = ifelse(is.na(dat[, vars[i]]), "Y", "N"), grp = dat[, by], cmp.method = "chisq")
                        tmp1 <- rbind(tmp1, c(tmp[4, ], as.vector(unlist(mis.by.grp[2, -1]))))
                    }
                    tmp <- tmp1
                }
            }
            out <- rbind(out, tmp)
        }
    }
    out <- data.frame(out)
    if (all) {
        dimnames(out)[[2]][1:2] <- c("Variables", paste("All(n=", n, ")", sep = ""))
        if (!is.null(by)) 
            dimnames(out)[[2]][3:(2 + n.level + 1)] <- c(paste(level.by, rep("(n=", n.level), n.by.cat, rep(")", n.level), sep = ""), "p.value")
    }
    else {
        dimnames(out)[[2]][1] <- "Variables"
        if (!is.null(by)) 
            dimnames(out)[[2]][2:(1 + n.level + 1)] <- c(paste(level.by, rep("(n=", n.level), n.by.cat, rep(")", n.level), sep = ""), "p.value")
    }
    if (!is.null(by)) 
        return(list(demographic = out, summary.all = out.all, 
                    summary.by.grp = out.by.grp))
    else return(list(demographic = out, summary.all = out.all))
}
