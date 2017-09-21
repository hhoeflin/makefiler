
Rule <- R6::R6Class("Rule",
                    public=list(
                        initialize=function(targets, phony=NULL, target_types=NULL,  prereqs, prereq_types=NULL, recipe) {
                            if(is.null(target_types)) {
                            target_types <- rep(NA, length(targets))
                            }
                            if(is.null(phony)) {
                                phony <- rep(FALSE, length(targets))
                            }
                            self$targets <- dplyr::tibble(name=targets, type=target_types, phony=phony)
                            if(is.null(prereq_types)) {
                            prereq_types <- rep(NA, length(prereqs))
                            }
                            self$prereqs <- dplyr::tibble(name=prereqs, type=prereq_types)
                            self$recipe <- recipe
                            
                            return(invisible(self))
                        },
                        get_rule=function() {
                            target_prereq <- paste(c(paste(self$targets$name, collapse=" "), paste(self$prereqs$name, collapse=" ")), collapse=": ")
                            recipe_tab <- paste("\t", self$recipe, sep="")
                            return(paste(c(target_prereq, recipe_tab), collapse="\n"))
                        },
                        get_targets=function() {
                            return(self$targets$name)
                        },
                        get_phony=function() {
                            return(self$targets$name[self$targets$phony])
                        },
                        get_prereqs=function() {
                            return(self$prereqs$name)
                        },
                        get_recipe=function() {
                            return(self$recipe)
                        },
                        get_node_df_noID=function() {
                            n <- length(c(self$targets$name, self$prereqs$name))
                            return(DiagrammeR::create_node_df(n=n, label=c(self$targets$name, self$prereqs$name)))
                        },
                        get_edge_df_noID=function() {
                            return(dplyr::as_tibble(expand.grid(from_label=self$targets$name, to_label=self$prereqs$name, stringsAsFactors = FALSE)))
                        },
                        targets=NULL,
                        prereqs=NULL,
                        recipe=NULL
                    )
                    )

