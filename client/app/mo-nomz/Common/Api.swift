//
//  Api.swift
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
            let onError = {
                self.alertUnsuccessful("Failed to initialize. Please close the app and try again.")
            }
            let onSuccess = { (data: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data)
                    completion?(output)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
    
    func pingUser(completion: (() -> Void)?) {
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/ping")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as! String
        let bundle = Bundle.main.infoDictionary?["CFBundleVersion"] as! String
        req.httpBody = try? JSONEncoder().encode(UserPingRequest(version: "\(version).\(bundle)"))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            completion?()
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
            let onComplete = {
                User.setDidExport()
                completion?()
            }
            let onSuccess = { (data: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ExportResponse.self, from: data)
                    Database.overwrite(export: output)
                    onComplete()
                } catch {
                    onComplete()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onComplete)
        })
        task.resume()
    }
    
    func pullSteps(completion: (() -> Void)?) {
        let progress = startProgress()
        guard let state = User.loadState() else { return }
        let recipes = Database.selectRecipes()
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
                        let _ = Database.addRecipeSteps(recipeId: id, rawSteps: output.steps)
                    } catch { }
                    onComplete()
                }
                self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onComplete)
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
        let recipes = Database.selectRecipes()
        for recipe in recipes {
            var steps = Set<String>()
            for (stepId, step) in recipe.recipe.steps.sorted(by: { $0.value.order < $1.value.order }) {
                if !steps.insert(step.step).inserted {
                    Database.deleteRecipeStep(id: stepId)
                }
            }
        }
        stopLoading(progress)
        completion?()
    }
    
    func addBlob(content: String, completion: (([ReadableIngredient]) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = {
                let rawIngredients = content.components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
                var ingredients = [ReadableIngredient]()
                var order = 0
                for ingredient in rawIngredients {
                    ingredients.append(ReadableIngredient(name: ingredient, quantity: ReadableQuantity(whole: nil, fraction: nil), unit: nil, order: order))
                    order += 1
                }
                Database.insertGroceries(ingredients: ingredients)
                completion?(ingredients)
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    Database.insertGroceries(ingredients: output.ingredients)
                    completion?(output.ingredients)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
    
    func addBlob(content: String, name: String, link: String?, rawSteps: [String], active: Bool, completion: ((ReadableRecipeWithId) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = {
                let rawIngredients = content.components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
                var ingredients = [ReadableIngredient]()
                var order = 0
                for ingredient in rawIngredients {
                    ingredients.append(ReadableIngredient(name: ingredient, quantity: ReadableQuantity(whole: nil, fraction: nil), unit: nil, order: order))
                    order += 1
                }
                let recipe = Database.insertRecipe(response: ParseBlobResponse(ingredients: ingredients), name: name, link: link, rawSteps: rawSteps, active: active, tags: [])
                completion?(recipe)
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    let recipe = Database.insertRecipe(response: output, name: name, link: link, rawSteps: rawSteps, active: active, tags: [])
                    completion?(recipe)
                } catch {
                    
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
    
    func addLink(link: String, active: Bool, completion: ((ReadableRecipeWithId) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = { self.alertUnsuccessful("Unable to import recipe. Please select photos or enter ingredients manually.")
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseLinkResponse.self, from: d)
                    let recipe = Database.insertRecipe(response: output, link: link, active: active, tags: [])
                    completion?(recipe)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
}
