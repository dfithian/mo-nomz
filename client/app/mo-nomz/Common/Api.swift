//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import UIKit
import Foundation

extension UIViewController {
    func loadUser(completion: ((CreateUserResponse) -> Void)?) {
        let spinner = startLoading()
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: { (d) -> Void in
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: d)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadExport(completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/export")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "GET"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: { (d) -> Void in
                do {
                    let output = try JSONDecoder().decode(ExportResponse.self, from: d)
                    self.overwrite(export: output)
                    User.setDidExport()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func pullSteps(completion: (() -> Void)?) {
        let progress = startProgress()
        guard let state = User.loadState() else { return }
        let recipes = selectRecipes()
        let queue = DispatchQueue(label: "mo-nomz.pull-steps")
        let group = DispatchGroup()
        var numPulled = 0
        let links = recipes.compactMap({ (recipe) -> (UUID, String)? in
            if let link = recipe.recipe.link {
                return (recipe.id, link)
            }
            return nil
        })
        for (id, link) in links {
            group.enter()
            var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
            req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
            req.addValue("application/json", forHTTPHeaderField: "Content-Type")
            req.httpMethod = "POST"
            req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
            let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
                let onComplete = {
                    numPulled += 1
                    DispatchQueue.main.async {
                        progress.setProgress(Float(numPulled) / Float(links.count), animated: true)
                    }
                    group.leave()
                }
                let onSuccess = { (data: Data) -> Void in
                    do {
                        let output = try JSONDecoder().decode(ParseLinkResponse.self, from: data)
                        let _ = self.addRecipeSteps(recipeId: id, rawSteps: output.steps)
                    } catch { }
                    onComplete()
                }
                self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: { _ in onComplete() }, onError: { _ in onComplete() })
            })
            let delay = Double.random(in: 0...(Double(links.count) / 2))
            queue.asyncAfter(deadline: .now() + delay) {
                task.resume()
            }
        }
        group.notify(queue: .main) {
            self.stopLoading(progress)
            User.setDidPullSteps()
            completion?()
        }
    }
    
    func cleanSteps(completion: (() -> Void)?) {
        let progress = startProgress()
        let recipes = selectRecipes()
        for recipe in recipes {
            var steps = Set<String>()
            for (stepId, step) in recipe.recipe.steps.sorted(by: { $0.value.order < $1.value.order }) {
                if !steps.insert(step.step).inserted {
                    deleteRecipeStep(id: stepId)
                }
            }
        }
        stopLoading(progress)
        completion?()
    }
    
    func addBlob(content: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    self.insertGroceries(ingredients: output.ingredients)
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addBlob(content: String, name: String, link: String?, rawSteps: [String], active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    var ingredients = [UUID:ReadableIngredient]()
                    for ingredient in output.ingredients {
                        ingredients[UUID()] = ingredient
                    }
                    var order = 0
                    var steps = [UUID:Step]()
                    for step in rawSteps {
                        steps[UUID()] = Step(step: step, order: order)
                        order += 1
                    }
                    let recipe = ReadableRecipe(name: name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps)
                    self.insertRecipe(recipe: recipe)
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addLink(link: String, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Unable to import recipe. Please enter ingredients manually.")
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseLinkResponse.self, from: d)
                    var ingredients = [UUID:ReadableIngredient]()
                    for ingredient in output.ingredients {
                        ingredients[UUID()] = ingredient
                    }
                    var order = 0
                    var steps = [UUID:Step]()
                    for step in output.steps {
                        steps[UUID()] = Step(step: step, order: order)
                        order += 1
                    }
                    let recipe = ReadableRecipe(name: output.name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps)
                    self.insertRecipe(recipe: recipe)
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
}
